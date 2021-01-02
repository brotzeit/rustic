;;; rustic.el --- Rust development environment -*-lexical-binding: t-*-

;; Version: 1.3
;; Author: Mozilla
;;
;; Keywords: languages
;; Package-Requires: ((emacs "26.1") (xterm-color "1.6") (dash "2.13.0") (s "1.10.0") (f "0.18.2") (markdown-mode "2.3") (spinner "1.7.3") (let-alist "1.0.4") (seq "2.3") (ht "2.0"))

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

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(require 'json)

(require 'rustic-common)

(defvar electric-pair-inhibit-predicate)
(defvar electric-indent-chars)

(defvar rustic-buffer-workspace-dir nil)
(make-variable-buffer-local 'rustic-buffer-workspace-dir)

;;; Customization

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

(defcustom rustic-always-locate-project-on-open nil
  "Whether to run `cargo locate-project' every time `rustic-mode' is activated."
  :type 'boolean
  :group 'rustic)

(defcustom rust-indent-return-type-to-arguments t
  "Indent a line starting with the `->' (RArrow) following a function, aligning
to the function arguments.  When nil, `->' will be indented one level."
  :type 'boolean
  :group 'rust-mode
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

;;; Rust-mode

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

(defvar rustic-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.  We suppress this with syntactic propertization
    ;; when needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table)
  "Syntax definitions and helpers.")

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
(define-derived-mode rustic-mode prog-mode "Rustic"
  "Major mode for Rust code.

\\{rustic-mode-map}"
  :group 'rustic
  :syntax-table rustic-mode-syntax-table

  ;; Syntax.
  (setq-local syntax-propertize-function #'rustic-syntax-propertize)

  ;; Indentation
  (setq-local indent-line-function 'rustic-indent-line)

  ;; Fonts
  (setq-local font-lock-defaults '(rustic-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function
                                    . rustic-syntactic-face-function)))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Auto indent on }
  (setq-local
   electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                       electric-indent-chars)))

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
              (concat "[[:space:]]*\\(?:" comment-start-skip "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local normal-auto-fill-function 'rustic-do-auto-fill)
  (setq-local fill-paragraph-function 'rustic-fill-paragraph)
  (setq-local fill-forward-paragraph-function 'rustic-fill-forward-paragraph)
  (setq-local adaptive-fill-function 'rustic-find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function 'rustic-comment-indent-new-line)
  (setq-local imenu-generic-expression rustic-imenu-generic-expression)
  (setq-local imenu-syntax-alist '((?! . "w"))) ; For macro_rules!
  (setq-local beginning-of-defun-function 'rustic-beginning-of-defun)
  (setq-local end-of-defun-function 'rustic-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-pair-inhibit-predicate 'rustic-electric-pair-inhibit-predicate-wrap)

  (add-hook 'before-save-hook 'rustic-before-save-hook nil t)
  (add-hook 'after-save-hook 'rustic-after-save-hook nil t)

  (setq-local rustic-buffer-workspace-dir nil)

  (when rustic-always-locate-project-on-open
    (setq-local rustic-buffer-workspace-dir (rustic-buffer-workspace)))
  (when rustic-lsp-setup-p
    (rustic-setup-lsp)))

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

(defun rustic-path-font-lock-matcher (re-ident)
  "Match occurrences of RE-IDENT followed by a double-colon.
Examples include to match names like \"foo::\" or \"Foo::\".
Does not match type annotations of the form \"foo::<\"."
  `(lambda (limit)
     (catch 'rustic-path-font-lock-matcher
       (while t
         (let* ((symbol-then-colons (rx-to-string '(seq (group (regexp ,re-ident)) "::")))
                (match (re-search-forward symbol-then-colons limit t)))
           (cond
            ;; If we didn't find a match, there are no more occurrences
            ;; of foo::, so return.
            ((null match) (throw 'rustic-path-font-lock-matcher nil))
            ;; If this isn't a type annotation foo::<, we've found a
            ;; match, so a return it!
            ((not (looking-at (rx (0+ space) "<")))
             (throw 'rustic-path-font-lock-matcher match))))))))

(defvar rustic-font-lock-keywords
  (append
   `(
     ;; Keywords proper
     (,(regexp-opt rustic-keywords 'symbols) . font-lock-keyword-face)

     ;; Contextual keywords
     ("\\_<\\(default\\)[[:space:]]+fn\\_>" 1 font-lock-keyword-face)
     (,rustic-re-union 1 font-lock-keyword-face)

     ;; Special types
     (,(regexp-opt rustic-special-types 'symbols) . font-lock-type-face)

     ;; The unsafe keyword
     ("\\_<unsafe\\_>" . 'rustic-unsafe)

     ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
     (,(rustic-re-grab (concat "#\\!?\\[" rustic-re-ident "[^]]*\\]"))
      1 font-lock-preprocessor-face keep)

     ;; Builtin formatting macros
     (,(concat (rustic-re-grab (concat (regexp-opt rustic-builtin-formatting-macros) "!"))
               rustic-formatting-macro-opening-re
               rustic-start-of-string-re)
      (1 'rustic-builtin-formatting-macro)
      (rustic-string-interpolation-matcher
       (rustic-end-of-string)
       nil
       (0 'rustic-string-interpolation t nil)))

     ;; write! macro
     (,(concat (rustic-re-grab "write\\(ln\\)?!")
               rustic-formatting-macro-opening-re
               "[[:space:]]*[^\"]+,[[:space:]]*"
               rustic-start-of-string-re)
      (1 'rustic-builtin-formatting-macro)
      (rustic-string-interpolation-matcher
       (rustic-end-of-string)
       nil
       (0 'rustic-string-interpolation t nil)))

     ;; Syntax extension invocations like `foo!`, highlight including the !
     (,(concat (rustic-re-grab (concat rustic-re-ident "!")) "[({[:space:][]")
      1 font-lock-preprocessor-face)

     ;; Field names like `foo:`, highlight excluding the :
     (,(concat (rustic-re-grab rustic-re-ident) ":[^:]") 1 font-lock-variable-name-face)

     ;; CamelCase Means Type Or Constructor
     (,rustic-re-type-or-constructor 1 font-lock-type-face)

     ;; Type-inferred binding
     (,(concat "\\_<\\(?:let\\s-+ref\\|let\\|ref\\)\\s-+\\(?:mut\\s-+\\)?"
               (rustic-re-grab rustic-re-ident)
               "\\_>")
      1 font-lock-variable-name-face)

     ;; Type names like `Foo::`, highlight excluding the ::
     (,(rustic-path-font-lock-matcher rustic-re-uc-ident) 1 font-lock-type-face)

     ;; Module names like `foo::`, highlight excluding the ::
     (,(rustic-path-font-lock-matcher rustic-re-lc-ident) 1 font-lock-constant-face)

     ;; Lifetimes like `'foo`
     (,(concat "'" (rustic-re-grab rustic-re-ident) "[^']") 1 font-lock-variable-name-face)

     ;; Question mark operator
     ("\\?" . 'rustic-question-mark)
     )

   ;; Ensure we highlight `Foo` in `struct Foo` as a type.
   (mapcar #'(lambda (x)
               (list (rustic-re-item-def (car x))
                     1 (cdr x)))
           '(("enum" . font-lock-type-face)
             ("struct" . font-lock-type-face)
             ("union" . font-lock-type-face)
             ("type" . font-lock-type-face)
             ("mod" . font-lock-constant-face)
             ("use" . font-lock-constant-face)
             ("fn" . font-lock-function-name-face)))))

(defun rustic-looking-back-str (str)
  "Return non-nil if there's a match on the text before point and STR.
Like `looking-back' but for fixed strings rather than regexps (so
that it's not so slow)."
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun rustic-looking-back-symbols (symbols)
  "Return non-nil if the point is after a member of SYMBOLS.
SYMBOLS is a list of strings that represent the respective
symbols."
  (save-excursion
    (let* ((pt-orig (point))
           (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
           (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
      (and
       (= end-of-symbol pt-orig)
       (member (buffer-substring-no-properties beg-of-symbol pt-orig)
               symbols)))))

(defun rustic-looking-back-ident ()
  "Non-nil if we are looking backwards at a valid rust identifier."
  (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
    (looking-back rustic-re-ident beg-of-symbol)))

(defvar-local rustic-macro-scopes nil
  "Cache for the scopes calculated by `rustic-macro-scope'.
This variable can be `let' bound directly or indirectly around
`rustic-macro-scope' as an optimization but should not be otherwise
set.")

(defun rustic-macro-scope (start end)
  "Return the scope of macros in the buffer.
The return value is a list of (START END) positions in the
buffer.
If set START and END are optimizations which limit the return
value to scopes which are approximately with this range."
  (save-excursion
    ;; need to special case macro_rules which has unique syntax
    (let ((scope nil)
          (start (or start (point-min)))
          (end (or end (point-max))))
      (goto-char start)
      ;; if there is a start move back to the previous top level,
      ;; as any macros before that must have closed by this time.
      (let ((top (syntax-ppss-toplevel-pos (syntax-ppss))))
        (when top
          (goto-char top)))
      (while
          (and
           ;; The movement below may have moved us passed end, in
           ;; which case search-forward will error
           (< (point) end)
           (search-forward "!" end t))
        (let ((pt (point)))
          (cond
           ;; in a string or comment is boring, move straight on
           ((rustic-in-str-or-cmnt))
           ;; in a normal macro,
           ((and (skip-chars-forward " \t\n\r")
                 (memq (char-after)
                       '(?\[ ?\( ?\{))
                 ;; Check that we have a macro declaration after.
                 (rustic-looking-back-macro))
            (let ((start (point)))
              (ignore-errors (forward-list))
              (setq scope (cons (list start (point)) scope))))
           ;; macro_rules, why, why, why did you not use macro syntax??
           ((save-excursion
              ;; yuck -- last test moves point, even if it fails
              (goto-char (- pt 1))
              (skip-chars-backward " \t\n\r")
              (rustic-looking-back-str "macro_rules"))
            (save-excursion
              (when (re-search-forward "[[({]" nil t)
                (backward-char)
                (let ((start (point)))
                  (ignore-errors (forward-list))
                  (setq scope (cons (list start (point)) scope)))))))))
      ;; Return 'empty rather than nil, to indicate a buffer with no
      ;; macros at all.
      (or scope 'empty))))

(defun rustic-looking-back-macro ()
  "Non-nil if looking back at an ident followed by a !"
  "Non-nil if looking back at an ident followed by a !
This is stricter than rust syntax which allows a space between
the ident and the ! symbol. If this space is allowed, then we
would also need a keyword check to avoid `if !(condition)` being
seen as a macro."
  (if (> (- (point) (point-min)) 1)
      (save-excursion
        (backward-char)
        (and (= ?! (char-after))
             (rustic-looking-back-ident)))))

(defun rustic-paren-level () (nth 0 (syntax-ppss)))
(defun rustic-in-str () (nth 3 (syntax-ppss)))
(defun rustic-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun rustic-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

(defun rustic-rewind-irrelevant ()
  (let ((continue t))
    (while continue
      (let ((starting (point)))
        (skip-chars-backward "[:space:]\n")
        (when (rustic-looking-back-str "*/")
          (backward-char))
        (when (rustic-in-str-or-cmnt)
          (rustic-rewind-past-str-cmnt))
        ;; Rewind until the point no longer moves
        (setq continue (/= starting (point)))))))

(defun rustic-in-macro (&optional start end)
  "Return non-nil when point is within the scope of a macro.
If START and END are set, minimize the buffer analysis to
approximately this location as an optimization.
Alternatively, if `rustic-macro-scopes' is a list use the scope
information in this variable. This last is an optimization and
the caller is responsible for ensuring that the data in
`rustic-macro-scopes' is up to date."
  (when (> (rustic-paren-level) 0)
    (let ((scopes
           (or
            rustic-macro-scopes
            (rustic-macro-scope start end))))
      ;; `rustic-macro-scope' can return the symbol `empty' if the
      ;; buffer has no macros at all.
      (when (listp scopes)
        (seq-some
         (lambda (sc)
           (and (>= (point) (car sc))
                (< (point) (cadr sc))))
         scopes)))))

(defun rustic-looking-at-where ()
  "Return T when looking at the \"where\" keyword."
  (and (looking-at-p "\\bwhere\\b")
       (not (rustic-in-str-or-cmnt))))

(defun rustic-rewind-to-where (&optional limit)
  "Rewind the point to the closest occurrence of the \"where\" keyword.
Return T iff a where-clause was found.  Does not rewind past
LIMIT when passed, otherwise only stops at the beginning of the
buffer."
  (when (re-search-backward "\\bwhere\\b" limit t)
    (if (rustic-in-str-or-cmnt)
        (rustic-rewind-to-where limit)
      t)))

(defconst rustic-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")

(defconst rustic-re-special-types (regexp-opt rustic-special-types 'symbols))

(defun rustic-next-string-interpolation (limit)
  "Search forward from point for next Rust interpolation marker before LIMIT.
Set point to the end of the occurrence found, and return match beginning
and end."
  (catch 'match
    (save-match-data
      (save-excursion
        (while (search-forward "{" limit t)
          (if (eql (char-after (point)) ?{)
              (forward-char)
            (let ((start (match-beginning 0)))
              ;; According to fmt_macros::Parser::next, an opening brace
              ;; must be followed by an optional argument and/or format
              ;; specifier, then a closing brace. A single closing brace
              ;; without a corresponding unescaped opening brace is an
              ;; error. We don't need to do anything special with
              ;; arguments, specifiers, or errors, so we only search for
              ;; the single closing brace.
              (when (search-forward "}" limit t)
                (throw 'match (list start (point)))))))))))

(defun rustic-string-interpolation-matcher (limit)
  "Match next Rust interpolation marker before LIMIT and set match data if found.
Returns nil if not within a Rust string."
  (when (rustic-in-str)
    (let ((match (rustic-next-string-interpolation limit)))
      (when match
        (set-match-data match)
        (goto-char (cadr match))
        match))))

(defun rustic-syntax-class-before-point ()
  (when (> (point) 1)
    (syntax-class (syntax-after (1- (point))))))

(defun rustic-rewind-qualified-ident ()
  (while (rustic-looking-back-ident)
    (backward-sexp)
    (when (save-excursion (rustic-rewind-irrelevant) (rustic-looking-back-str "::"))
      (rustic-rewind-irrelevant)
      (backward-char 2)
      (rustic-rewind-irrelevant))))

(defun rustic-rewind-type-param-list ()
  (cond
   ((and (rustic-looking-back-str ">") (equal 5 (rustic-syntax-class-before-point)))
    (backward-sexp)
    (rustic-rewind-irrelevant))

   ;; We need to be able to back up past the Fn(args) -> RT form as well.  If
   ;; we're looking back at this, we want to end up just after "Fn".
   ((member (char-before) '(?\] ?\) ))
    (let* ((is-paren (rustic-looking-back-str ")"))
           (dest (save-excursion
                   (backward-sexp)
                   (rustic-rewind-irrelevant)
                   (or
                    (when (rustic-looking-back-str "->")
                      (backward-char 2)
                      (rustic-rewind-irrelevant)
                      (when (rustic-looking-back-str ")")
                        (backward-sexp)
                        (point)))
                    (and is-paren (point))))))
      (when dest
        (goto-char dest))))))

(defun rustic-rewind-to-decl-name ()
  "Return the point at the beginning of the name in a declaration.
I.e. if we are before an ident that is part of a declaration that
can have a where clause, rewind back to just before the name of
the subject of that where clause and return the new point.
Otherwise return nil."
  (let* ((ident-pos (point))
         (newpos (save-excursion
                   (rustic-rewind-irrelevant)
                   (rustic-rewind-type-param-list)
                   (cond
                    ((rustic-looking-back-symbols
                      '("fn" "trait" "enum" "struct" "union" "impl" "type"))
                     ident-pos)

                    ((equal 5 (rustic-syntax-class-before-point))
                     (backward-sexp)
                     (rustic-rewind-to-decl-name))

                    ((looking-back "[:,'+=]" (1- (point)))
                     (backward-char)
                     (rustic-rewind-to-decl-name))

                    ((rustic-looking-back-str "->")
                     (backward-char 2)
                     (rustic-rewind-to-decl-name))

                    ((rustic-looking-back-ident)
                     (rustic-rewind-qualified-ident)
                     (rustic-rewind-to-decl-name))))))
    (when newpos (goto-char newpos))
    newpos))

(defun rustic-is-in-expression-context (token)
  "Return t if what comes right after the point is part of an
expression (as opposed to starting a type) by looking at what
comes before.  Takes a symbol that roughly indicates what is
after the point.

This function is used as part of `rustic-is-lt-char-operator' as
part of angle bracket matching, and is not intended to be used
outside of this context."
  (save-excursion
    (let ((postchar (char-after)))
      (rustic-rewind-irrelevant)
      ;; A type alias or ascription could have a type param list.  Skip backwards past it.
      (when (member token '(ambiguous-operator open-brace))
        (rustic-rewind-type-param-list))
      (cond

       ;; Certain keywords always introduce expressions
       ((rustic-looking-back-symbols '("if" "while" "match" "return" "box" "in")) t)

       ;; "as" introduces a type
       ((rustic-looking-back-symbols '("as")) nil)

       ;; An open angle bracket never introduces expression context WITHIN the angle brackets
       ((and (equal token 'open-brace) (equal postchar ?<)) nil)

       ;; An ident! followed by an open brace is a macro invocation.  Consider
       ;; it to be an expression.
       ((and (equal token 'open-brace) (rustic-looking-back-macro)) t)

       ;; In a brace context a "]" introduces an expression.
       ((and (eq token 'open-brace) (rustic-looking-back-str "]")))

       ;; An identifier is right after an ending paren, bracket, angle bracket
       ;; or curly brace.  It's a type if the last sexp was a type.
       ((and (equal token 'ident) (equal 5 (rustic-syntax-class-before-point)))
        (backward-sexp)
        (rustic-is-in-expression-context 'open-brace))

       ;; If a "for" appears without a ; or { before it, it's part of an
       ;; "impl X for y", so the y is a type.  Otherwise it's
       ;; introducing a loop, so the y is an expression
       ((and (equal token 'ident) (rustic-looking-back-symbols '("for")))
        (backward-sexp)
        (rustic-rewind-irrelevant)
        (looking-back "[{;]" (1- (point))))

       ((rustic-looking-back-ident)
        (rustic-rewind-qualified-ident)
        (rustic-rewind-irrelevant)
        (cond
         ((equal token 'open-brace)
          ;; We now know we have:
          ;;   ident <maybe type params> [{([]
          ;; where [{([] denotes either a {, ( or [.
          ;; This character is bound as postchar.
          (cond
           ;; If postchar is a paren or square bracket, then if the
           ;; brace is a type if the identifier is one
           ((member postchar '(?\( ?\[ )) (rustic-is-in-expression-context 'ident))

           ;; If postchar is a curly brace, the brace can only be a type if
           ;; ident2 is the name of an enum, struct or trait being declared.
           ;; Note that if there is a -> before the ident then the ident would
           ;; be a type but the { is not.
           ((equal ?{ postchar)
            (not (and (rustic-rewind-to-decl-name)
                      (progn
                        (rustic-rewind-irrelevant)
                        (rustic-looking-back-symbols
                         '("enum" "struct" "union" "trait" "type"))))))))

         ((equal token 'ambiguous-operator)
          (cond
           ;; An ampersand after an ident has to be an operator rather
           ;; than a & at the beginning of a ref type
           ((equal postchar ?&) t)

           ;; A : followed by a type then an = introduces an
           ;; expression (unless it is part of a where clause of a
           ;; "type" declaration)
           ((and (equal postchar ?=)
                 (looking-back "[^:]:" (- (point) 2))
                 (not (save-excursion
                        (and (rustic-rewind-to-decl-name)
                             (progn (rustic-rewind-irrelevant)
                                    (rustic-looking-back-symbols '("type"))))))))

           ;; "let ident =" introduces an expression--and so does "const" and "mut"
           ((and (equal postchar ?=) (rustic-looking-back-symbols '("let" "const" "mut"))) t)

           ;; As a specific special case, see if this is the = in this situation:
           ;;     enum EnumName<type params> { Ident =
           ;; In this case, this is a c-like enum and despite Ident
           ;; representing a type, what comes after the = is an expression
           ((and
             (> (rustic-paren-level) 0)
             (save-excursion
               (backward-up-list)
               (rustic-rewind-irrelevant)
               (rustic-rewind-type-param-list)
               (and
                (rustic-looking-back-ident)
                (progn
                  (rustic-rewind-qualified-ident)
                  (rustic-rewind-irrelevant)
                  (rustic-looking-back-str "enum")))))
            t)

           ;; Otherwise the ambiguous operator is a type if the identifier is a type
           ((rustic-is-in-expression-context 'ident) t)))

         ((equal token 'colon)
          (cond
           ;; If we see a ident: not inside any braces/parens, we're at top level.
           ;; There are no allowed expressions after colons there, just types.
           ((<= (rustic-paren-level) 0) nil)

           ;; We see ident: inside a list
           ((looking-back "[{,]" (1- (point)))
            (backward-up-list)

            ;; If a : appears whose surrounding paren/brackets/braces are
            ;; anything other than curly braces, it can't be a field
            ;; initializer and must be denoting a type.
            (when (looking-at "{")
              (rustic-rewind-irrelevant)
              (rustic-rewind-type-param-list)
              (when (rustic-looking-back-ident)
                ;; We have a context that looks like this:
                ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                ;; the point is sitting just after ident2, and we trying to
                ;; figure out if the colon introduces an expression or a type.
                ;; The answer is that ident1 is a field name, and what comes
                ;; after the colon is an expression, if ident2 is an
                ;; expression.
                (rustic-rewind-qualified-ident)
                (rustic-is-in-expression-context 'ident))))

           ;; Otherwise, if the ident: appeared with anything other than , or {
           ;; before it, it can't be part of a struct initializer and therefore
           ;; must be denoting a type.
           (t nil)))))

       ;; An operator-like character after a string is indeed an operator
       ((and (equal token 'ambiguous-operator)
             (member (rustic-syntax-class-before-point) '(5 7 15))) t)

       ;; A colon that has something other than an identifier before it is a
       ;; type ascription
       ((equal token 'colon) nil)

       ;; A :: introduces a type (or module, but not an expression in any case)
       ((rustic-looking-back-str "::") nil)

       ((rustic-looking-back-str ":")
        (backward-char)
        (rustic-is-in-expression-context 'colon))

       ;; A -> introduces a type
       ((rustic-looking-back-str "->") nil)

       ;; If we are up against the beginning of a list, or after a comma inside
       ;; of one, back up out of it and check what the list itself is
       ((or
         (equal 4 (rustic-syntax-class-before-point))
         (rustic-looking-back-str ","))
        (condition-case nil
            (progn
              (backward-up-list)
              (rustic-is-in-expression-context 'open-brace))
          (scan-error nil)))

       ;; A => introduces an expression
       ((rustic-looking-back-str "=>") t)

       ;; A == introduces an expression
       ((rustic-looking-back-str "==") t)

       ;; These operators can introduce expressions or types
       ((looking-back "[-+=!?&*]" (1- (point)))
        (backward-char)
        (rustic-is-in-expression-context 'ambiguous-operator))

       ;; These operators always introduce expressions.  (Note that if this
       ;; regexp finds a < it must not be an angle bracket, or it'd
       ;; have been caught in the syntax-class check above instead of this.)
       ((looking-back rustic-re-pre-expression-operators (1- (point))) t)))))

(defun rustic-is-lt-char-operator ()
  "Return non-nil if the `<' sign just after point is an operator.
Otherwise, if it is an opening angle bracket, then return nil."
  (let ((case-fold-search nil))
    (save-excursion
      (rustic-rewind-irrelevant)
      ;; We are now just after the character syntactically before the <.
      (cond

       ;; If we are looking back at a < that is not an angle bracket (but not
       ;; two of them) then this is the second < in a bit shift operator
       ((and (rustic-looking-back-str "<")
             (not (equal 4 (rustic-syntax-class-before-point)))
             (not (rustic-looking-back-str "<<"))))

       ;; On the other hand, if we are after a closing paren/brace/bracket it
       ;; can only be an operator, not an angle bracket.  Likewise, if we are
       ;; after a string it's an operator.  (The string case could actually be
       ;; valid in rust for character literals.)
       ((member (rustic-syntax-class-before-point) '(5 7 15)) t)

       ;; If we are looking back at an operator, we know that we are at
       ;; the beginning of an expression, and thus it has to be an angle
       ;; bracket (starting a "<Type as Trait>::" construct.)
       ((looking-back rustic-re-pre-expression-operators (1- (point))) nil)

       ;; If we are looking back at a keyword, it's an angle bracket
       ;; unless that keyword is "self", "true" or "false"
       ((rustic-looking-back-symbols rustic-keywords)
        (rustic-looking-back-symbols '("self" "true" "false")))

       ((rustic-looking-back-str "?")
        (rustic-is-in-expression-context 'ambiguous-operator))

       ;; If we're looking back at an identifier, this depends on whether
       ;; the identifier is part of an expression or a type
       ((rustic-looking-back-ident)
        (backward-sexp)
        (or
         ;; The special types can't take type param lists, so a < after one is
         ;; always an operator
         (looking-at rustic-re-special-types)

         (rustic-is-in-expression-context 'ident)))

       ;; Otherwise, assume it's an angle bracket
       ))))

(defun rustic-electric-pair-inhibit-predicate-wrap (char)
  "Prevent \"matching\" with a `>' when CHAR is the less-than operator.
This wraps the default defined by `electric-pair-inhibit-predicate'."
  (or
   (when (= ?< char)
     (save-excursion
       (backward-char)
       (rustic-is-lt-char-operator)))
   (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(defun rustic-ordinary-lt-gt-p ()
  "Test whether the `<' or `>' at point is an ordinary operator of some kind.

This returns t if the `<' or `>' is an ordinary operator (like
less-than) or part of one (like `->'); and nil if the character
should be considered a paired angle bracket."
  (cond
   ;; If matching is turned off suppress all of them
   ((not rustic-match-angle-brackets) t)

   ;; This is a cheap check so we do it early.
   ;; Don't treat the > in -> or => as an angle bracket
   ((and (= (following-char) ?>) (memq (preceding-char) '(?- ?=))) t)


   ;; We don't take < or > in strings or comments to be angle brackets
   ((rustic-in-str-or-cmnt) t)

   ;; Inside a macro we don't really know the syntax.  Any < or > may be an
   ;; angle bracket or it may not.  But we know that the other braces have
   ;; to balance regardless of the < and >, so if we don't treat any < or >
   ;; as angle brackets it won't mess up any paren balancing.
   ((rustic-in-macro) t)

   ((looking-at "<")
    (rustic-is-lt-char-operator))

   ((looking-at ">")
    (cond
     ;; Don't treat the > in -> or => as an angle bracket
     ((member (char-before (point)) '(?- ?=)) t)

     ;; If we are at top level and not in any list, it can't be a closing
     ;; angle bracket
     ((>= 0 (rustic-paren-level)) t)

     ;; Otherwise, treat the > as a closing angle bracket if it would
     ;; match an opening one
     ((save-excursion
        (backward-up-list)
        (not (looking-at "<"))))))))

(defun rustic-syntactic-face-function (state)
  "Return face that distinguishes doc and normal comments in given syntax STATE."
  (if (nth 3 state)
      'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face))))

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

(defun rustic--syntax-propertize-raw-string (str-start end)
  "A helper for rustic-syntax-propertize.

This will apply the appropriate string syntax to the character
from the STR-START up to the end of the raw string, or to END,
whichever comes first."
  (when (save-excursion
          (goto-char str-start)
          (looking-at "r\\(#*\\)\\(\"\\)"))
    ;; In a raw string, so try to find the end.
    (let ((hashes (match-string 1)))
      ;; Match \ characters at the end of the string to suppress
      ;; their normal character-quote syntax.
      (when (re-search-forward (concat "\\(\\\\*\\)\\(\"" hashes "\\)") end t)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "_"))
        (put-text-property (1- (match-end 2)) (match-end 2)
                           'syntax-table (string-to-syntax "|"))
        (goto-char (match-end 0))))))

(defun rustic-syntax-propertize (start end)
  "A `syntax-propertize-function' to apply properties from START to END."
  (let ((rustic-macro-scopes (rustic-macro-scope start end)))
    (goto-char start)
    (let ((str-start (rustic-in-str-or-cmnt)))
      (when str-start
        (rustic--syntax-propertize-raw-string str-start end)))
    (funcall
     (syntax-propertize-rules
      ;; Character literals.
      (rustic--char-literal-rx (1 "\"") (2 "\""))
      ;; Raw strings.
      ("\\(r\\)#*\""
       (0 (ignore
           (goto-char (match-end 0))
           (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
             (put-text-property (match-beginning 1) (match-end 1)
                                'syntax-table (string-to-syntax "|"))
             (rustic--syntax-propertize-raw-string (match-beginning 0) end)))))
      ("[<>]"
       (0 (ignore
           (when (save-match-data
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (rustic-ordinary-lt-gt-p)))
             (put-text-property (match-beginning 0) (match-end 0)
                                'syntax-table (string-to-syntax "."))
             (goto-char (match-end 0)))))))
     (point) end)))

(defun rustic-fill-prefix-for-comment-start (line-start)
  "Determine what to use for `fill-prefix' based on the text at LINE-START."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+?\\)[!*]?"
          (lambda (s)
            ;; We want the * to line up with the first * of the
            ;; comment start
            (let ((offset (if (eq t
                                  (compare-strings "/*" nil nil
                                                   s
                                                   (- (length s) 2)
                                                   (length s)))
                              1 2)))
              (concat (make-string (- (length s) offset)
                                   ?\x20) "*")))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun rustic-in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the user
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (nth 4 (syntax-ppss)))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; We need this when we're moving the point around and then checking syntax
    ;; while doing paragraph fills, because the cache it uses isn't always
    ;; invalidated during this.
    (syntax-ppss-flush-cache 1)
    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we use the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (let ((next-bol (line-beginning-position 2)))
      (while (save-excursion
               (end-of-line)
               (syntax-ppss-flush-cache 1)
               (and (nth 4 (syntax-ppss))
                    (save-excursion
                      (beginning-of-line)
                      (looking-at paragraph-start))
                    (looking-at "[[:space:]]*$")
                    (nth 4 (syntax-ppss next-bol))))
        (goto-char next-bol)))

    (syntax-ppss-flush-cache 1)
    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun rustic-with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (nth 4 (syntax-ppss))
          (cond
           ;; If we're inside the comment and see a * prefix, use it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to use for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (rustic-fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun rustic-find-fill-prefix ()
  (rustic-in-comment-paragraph
   (lambda ()
     (rustic-with-comment-fill-prefix
      (lambda ()
        fill-prefix)))))

(defun rustic-fill-paragraph (&rest args)
  "Special wrapping for `fill-paragraph'.
This handles multi-line comments with a * prefix on each line."
  (rustic-in-comment-paragraph
   (lambda ()
     (rustic-with-comment-fill-prefix
      (lambda ()
        (let
            ((fill-paragraph-function
              (if (not (eq fill-paragraph-function 'rustic-fill-paragraph))
                  fill-paragraph-function))
             (fill-paragraph-handle-comment t))
          (apply 'fill-paragraph args)
          t))))))

(defun rustic-do-auto-fill (&rest args)
  "Special wrapping for `do-auto-fill'.
This handles multi-line comments with a * prefix on each line."
  (rustic-with-comment-fill-prefix
   (lambda ()
     (apply 'do-auto-fill args)
     t)))

(defun rustic-fill-forward-paragraph (arg)
  ;; This is to work around some funny behavior when a paragraph separator is
  ;; at the very top of the file and there is a fill prefix.
  (let ((fill-prefix nil)) (forward-paragraph arg)))

(defun rustic-comment-indent-new-line (&optional arg)
  (rustic-with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))

(defun rustic-end-of-string ()
  "Skip to the end of the current string."
  (save-excursion
    (skip-syntax-forward "^\"|")
    (skip-syntax-forward "\"|")
    (point)))

(defun rustic-before-save-hook ()
  "Don't throw error if rustfmt isn't installed, as it makes saving impossible."
  (when (and (rustic-format-on-save-p) (not (rustic-compilation-process-live t)))
    (condition-case nil
        (progn
          (rustic-format-file)
          (sit-for 0.1))
      (error nil))))

(defun rustic-after-save-hook ()
  "Check if rustfmt is installed after saving the file."
  (when (rustic-format-on-save-p)
    (unless (executable-find rustic-rustfmt-bin)
      (error "Could not locate executable \"%s\"" rustic-rustfmt-bin))))

;;; _

(defun rustic-reload ()
  "Reload rustic package."
  (interactive)
  (unload-feature 'rustic)
  (require 'rustic)
  (rustic-mode))

(provide 'rustic)

(require 'rustic-util)
(require 'rustic-compile)
(require 'rustic-popup)
(require 'rustic-cargo)
(require 'rustic-babel)
(require 'rustic-racer)
(require 'rustic-interaction)

(with-eval-after-load 'flycheck
  (require 'rustic-flycheck))

(with-eval-after-load 'eglot
  (rustic-setup-eglot))

;;; rustic.el ends here
