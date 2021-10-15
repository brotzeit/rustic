;;; rustic.el --- Rust development environment -*-lexical-binding: t-*-

;; Version: 1.3
;; Author: Mozilla
;;
;; Keywords: languages
;; Package-Requires: ((emacs "26.1") (rust-mode "0.5.0") (dash "2.13.0") (f "0.18.2") (let-alist "1.0.4") (markdown-mode "2.3") (project "0.3.0") (s "1.10.0") (seq "2.3") (spinner "1.7.3") (xterm-color "1.6"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:

;; This package is a fork of rust-mode.
;;
;; Differences with rust-mode:
;;
;; - rust-analyzer configuration
;; - flycheck integration
;; - cargo popup
;; - multiline error parsing
;; - translation of ANSI control sequences through xterm-color
;; - async org babel
;; - custom compilation process
;; - rustfmt errors in a rust compilation mode
;; - automatic RLS configuration with eglot or lsp-mode
;; - cask for testing
;; - requires emacs 26
;; - etc.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(require 'dash)

(setq rust-load-optional-libraries nil)
(require 'rust-mode)

(eval-when-compile (require 'rx))

(defvar electric-pair-inhibit-predicate)
(defvar electric-pair-skip-self)
(defvar electric-indent-chars)

;;; Customization

(defgroup rustic nil
  "Support for Rust code."
  :link '(url-link "https://www.rustic-lang.org/")
  :group 'languages)

(defcustom rustic-indent-offset 4
  "Indent Rust code by this number of spaces."
  :type 'integer
  :group 'rustic
  :safe #'integerp)

(defcustom rustic-indent-method-chain nil
  "Indent Rust method chains, aligned by the `.' operators."
  :type 'boolean
  :group 'rustic
  :safe #'booleanp)

(defcustom rustic-indent-where-clause nil
  "Indent lines starting with the `where' keyword following a function or trait.
When nil, `where' will be aligned with `fn' or `trait'."
  :type 'boolean
  :group 'rustic
  :safe #'booleanp)

(defcustom rustic-match-angle-brackets t
  "Whether to enable angle bracket (`<' and `>') matching where appropriate."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

(defcustom rustic-indent-return-type-to-arguments t
  "Indent a line starting with the `->' (RArrow) following a function, aligning
to the function arguments.  When nil, `->' will be indented one level."
  :type 'boolean
  :group 'rustic
  :safe #'booleanp)

;;; Faces

(define-obsolete-face-alias 'rustic-unsafe-face
  'rustic-unsafe "1.2")
(define-obsolete-face-alias 'rustic-question-mark-face
  'rustic-question-mark "1.2")
(define-obsolete-face-alias 'rustic-builtin-formatting-macro-face
  'rustic-builtin-formatting-macro "1.2")
(define-obsolete-face-alias 'rustic-string-interpolation-face
  'rustic-string-interpolation "1.2")

(defface rustic-unsafe
  '((t :inherit font-lock-warning-face))
  "Face for the `unsafe' keyword."
  :group 'rustic)

(defface rustic-question-mark
  '((t :weight bold :inherit font-lock-builtin-face))
  "Face for the question mark operator."
  :group 'rustic)

(defface rustic-builtin-formatting-macro
  '((t :inherit font-lock-builtin-face))
  "Face for builtin formatting macros (print! &c.)."
  :group 'rustic)

(defface rustic-string-interpolation
  '((t :slant italic :inherit font-lock-string-face))
  "Face for interpolating braces in builtin formatting macro strings."
  :group 'rustic)

;;; Workspace

(defvar-local rustic--buffer-workspace nil
  "Use function `rustic-buffer-workspace' instead.")

(defun rustic-buffer-workspace (&optional nodefault)
  "Return the Rust workspace for the current buffer.
This is the directory containing the file \"Cargo.lock\".  When
called outside a Rust project, then return `default-directory',
or if NODEFAULT is non-nil, then fall back to returning nil."
  (or rustic--buffer-workspace
      (let ((dir (locate-dominating-file default-directory "Cargo.toml")))
        (when dir
          (setq dir (expand-file-name dir)))
        (setq rustic--buffer-workspace dir)
        (or dir
            (and (not nodefault)
                 default-directory)))))

;;; Syntax

(defconst rustic-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst rustic-re-lc-ident "[[:lower:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst rustic-re-uc-ident "[[:upper:]][[:word:][:multibyte:]_[:digit:]]*")
(defconst rustic-re-vis "pub")
(defconst rustic-re-unsafe "unsafe")
(defconst rustic-re-extern "extern")
(defconst rustic-re-generic
  (concat "<[[:space:]]*'" rustic-re-ident "[[:space:]]*>"))
(defconst rustic-re-union
  (rx-to-string
   `(seq
     (or space line-start)
     (group symbol-start "union" symbol-end)
     (+ space) (regexp ,rustic-re-ident))))

(defun rustic-re-shy (inner) (concat "\\(?:" inner "\\)"))
(defun rustic-re-grab (inner) (concat "\\(" inner "\\)"))
(defun rustic-re-item-def (itype)
  (concat (rustic-re-word itype)
          (rustic-re-shy rustic-re-generic) "?"
          "[[:space:]]+" (rustic-re-grab rustic-re-ident)))
(defun rustic-re-word (inner) (concat "\\<" inner "\\>"))

(defun rustic-re-item-def-imenu (itype)
  (concat "^[[:space:]]*"
          (rustic-re-shy (concat (rustic-re-word rustic-re-vis) "[[:space:]]+")) "?"
          (rustic-re-shy (concat (rustic-re-word "default") "[[:space:]]+")) "?"
          (rustic-re-shy (concat (rustic-re-word rustic-re-unsafe) "[[:space:]]+")) "?"
          (rustic-re-shy (concat (rustic-re-word rustic-re-extern) "[[:space:]]+"
                                 (rustic-re-shy "\"[^\"]+\"[[:space:]]+") "?")) "?"
          (rustic-re-item-def itype)))

(defvar rustic-imenu-generic-expression
  (append (mapcar #'(lambda (x)
                      (list (capitalize x) (rustic-re-item-def-imenu x) 1))
                  '("async fn" "enum" "struct" "union" "type" "mod" "fn" "trait" "impl"))
          `(("Macro" ,(rustic-re-item-def-imenu "macro_rules!") 1)))
  "Value for `imenu-generic-expression' in Rust mode.

Create a hierarchical index of the item definitions in a Rust file.

Imenu will show all the enums, structs, etc. in their own subheading.
Use idomenu (imenu with `ido-mode') for best mileage.")

;;; Mode

(defvar rustic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'rustic-popup)

    (define-key map (kbd "C-c C-c C-u") 'rustic-compile)
    (define-key map (kbd "C-c C-c C-i") 'rustic-recompile)
    (define-key map (kbd "C-c C-c C-b") 'rustic-cargo-build)
    (define-key map (kbd "C-c C-c C-k") 'rustic-cargo-check)
    (define-key map (kbd "C-c C-c C-r") 'rustic-cargo-run)
    (define-key map (kbd "C-c C-c C-f") 'rustic-cargo-fmt)
    (define-key map (kbd "C-c C-c C-t") 'rustic-cargo-test)
    (define-key map (kbd "C-c C-c C-c") 'rustic-cargo-current-test)
    (define-key map (kbd "C-c C-c C-l") 'rustic-cargo-clippy)
    (define-key map (kbd "C-c C-c C-o") 'rustic-format-buffer)

    (define-key map (kbd "C-c C-c C-d") 'rustic-racer-describe)
    (define-key map (kbd "C-c C-c C-,") 'rustic-docstring-dwim)
    (define-key map (kbd "C-c C-c C-n") 'rustic-cargo-outdated)
    map)
  "Keymap for Rust major mode.")

;;;###autoload
(define-derived-mode rustic-mode rust-mode "Rustic"
  "Major mode for Rust code.

\\{rustic-mode-map}"
  :group 'rustic

  (remove-hook 'before-save-hook 'rust-before-save-hook t)
  (remove-hook 'after-save-hook 'rust-after-save-hook t)

  ;; TODO: find out which function is up-to-date
  (setq rust-top-item-beg-re rustic-top-item-beg-re)

  (when (fboundp 'rustic-before-save-hook)
    (add-hook 'before-save-hook 'rustic-before-save-hook nil t)
    (add-hook 'after-save-hook 'rustic-after-save-hook nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

;; remove rust-mode from `auto-mode-alist'
(let ((mode '("\\.rs\\'" . rust-mode)))
  (when (member mode auto-mode-alist)
    (setq auto-mode-alist (remove mode auto-mode-alist))))

(defvar rustic-top-item-beg-re
  (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*"
          (regexp-opt
           '("enum" "struct" "union" "type" "mod" "use" "fn" "static" "impl"
             "extern" "trait" "async"))
          "\\_>")
  "Start of a Rust item.")

(defconst rustic-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst rustic-keywords
  '("as" "async" "await"
    "box" "break"
    "const" "continue" "crate"
    "do" "dyn"
    "else" "enum" "extern" "existential"
    "false" "fn" "for"
    "if" "impl" "in"
    "let" "loop"
    "match" "mod" "move" "mut"
    "priv" "pub"
    "ref" "return"
    "self" "static" "struct" "super"
    "true" "trait" "type" "try"
    "use"
    "virtual"
    "where" "while"
    "yield")
  "Font-locking definitions and helpers.")

(defconst rustic-special-types
  '("u8" "i8"
    "u16" "i16"
    "u32" "i32"
    "u64" "i64"
    "u128" "i128"

    "f32" "f64"
    "isize" "usize"
    "bool"
    "str" "char"))

(defvar rustic-builtin-formatting-macros
  '("eprint"
    "eprintln"
    "format"
    "print"
    "println")
  "List of builtin Rust macros for string formatting.
This is used by `rust-font-lock-keywords'.
\(`write!' is handled separately).")

(defvar rustic-formatting-macro-opening-re
  "[[:space:]]*[({[][[:space:]]*"
  "Regular expression to match the opening delimiter of a Rust formatting macro.")

(defvar rustic-start-of-string-re
  "\\(?:r#*\\)?\""
  "Regular expression to match the start of a Rust raw string.")

(eval-and-compile
  (defconst rustic--char-literal-rx
    (rx (seq
         (group "'")
         (or
          (seq
           "\\"
           (or
            (: "u{" (** 1 6 xdigit) "}")
            (: "x" (= 2 xdigit))
            (any "'nrt0\"\\")))
          (not (any "'\\")))
         (group "'")))
    "A regular expression matching a character literal."))

;;; _

(defun rustic-reload ()
  "Reload rustic package."
  (interactive)
  (unload-feature 'rustic)
  (require 'rustic)
  (rustic-mode))

(provide 'rustic)

(require 'rustic-interaction)

(defvar rustic-load-optional-libraries t
  "Whether loading `rustic' also loads optional libraries.
This variable might soon be remove again.")

(when rustic-load-optional-libraries
  (require 'rustic-compile)
  (require 'rustic-popup)
  (require 'rustic-cargo)
  (require 'rustic-babel)
  (require 'rustic-racer)
  (require 'rustic-rustfmt)
  (require 'rustic-rustfix)
  (require 'rustic-playpen)
  (require 'rustic-lsp)
  (with-eval-after-load 'flycheck
    (require 'rustic-flycheck)))

;;; rustic.el ends here
