;;; rustic.el --- Rust development environment -*-lexical-binding: t-*-

;; Version: 3.3
;; Author: Mozilla
;;
;; Keywords: languages
;; Package-Requires: ((emacs "26.1") (rust-mode "1.0.3") (dash "2.13.0") (f "0.18.2") (let-alist "1.0.4") (markdown-mode "2.3") (project "0.3.0") (s "1.10.0") (seq "2.3") (spinner "1.7.3") (xterm-color "1.6"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:

;; This package is based on rust-mode and provides additional features:
;;
;; - rust-analyzer configuration
;; - flycheck integration
;; - cargo popup
;; - multiline error parsing
;; - translation of ANSI control sequences through xterm-color
;; - async org babel
;; - custom compilation process
;; - rustfmt errors in a rust compilation mode
;; - automatic rust-analyzer configuration with eglot or lsp-mode
;; - cask for testing
;; - requires emacs 26
;; - and more

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(require 'dash)

(setq rust-load-optional-libraries nil)
(setq rust-before-save-hook #'rustic-before-save-hook)
(setq rust-after-save-hook #'rustic-after-save-hook)
(require 'rust-mode)

;;; Customization

(defgroup rustic nil
  "Support for Rust code."
  :link '(url-link "https://www.rustic-lang.org/")
  :group 'languages)

;;; Define aliases for removed rustic functions

(defvaralias 'rustic-indent-offset 'rust-indent-offset)
(defvaralias 'rustic-indent-method-chain 'rust-indent-method-chain)
(defvaralias 'rustic-indent-where-clause 'rust-indent-where-clause)
(defvaralias 'rustic-match-angle-brackets 'rust-match-angle-brackets)
(defvaralias 'rustic-indent-return-type-to-arguments 'rust-indent-return-type-to-arguments)
(defalias 'rustic-indent-line #'rust-mode-indent-line)
(defalias 'rustic-end-of-defun #'rust-end-of-defun)

;;; Workspace

(defvar-local rustic--buffer-workspace nil
  "Use function `rustic-buffer-workspace' instead.")

(defun rustic-buffer-workspace (&optional nodefault)
  "Get workspace for the current buffer."
  ;; this variable is buffer local so we can use the cached value
  (if rustic--buffer-workspace
      rustic--buffer-workspace
    ;; Copy environment variables into the new buffer, since
    ;; with-temp-buffer will re-use the variables' defaults, even if
    ;; they have been changed in this variable using e.g. envrc-mode.
    ;; See https://github.com/purcell/envrc/issues/12.
    (let ((env process-environment)
          (path exec-path))
      (with-temp-buffer
        ;; Copy the entire environment just in case there's something we
        ;; don't know we need.
        (setq-local process-environment env)
        ;; Set PATH so we can find cargo.
        (setq-local exec-path path)
        (let ((ret (call-process (rustic-cargo-bin) nil (list (current-buffer) nil) nil "locate-project" "--workspace")))
          (cond ((and (/= ret 0) nodefault)
                 (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
                ((and (/= ret 0) (not nodefault))
                 (setq rustic--buffer-workspace default-directory))
                (t
                 (goto-char 0)
                 (let* ((output (json-read))
                        (dir (file-name-directory (cdr (assoc-string "root" output)))))
                   (setq rustic--buffer-workspace dir)))))))))

(defun rustic-buffer-crate (&optional nodefault)
  "Return the crate for the current buffer.
When called outside a Rust project, then return `default-directory',
or if NODEFAULT is non-nil, then fall back to returning nil."
  (let ((dir (locate-dominating-file default-directory "Cargo.toml")))
    (when dir
      (setq dir (expand-file-name dir)))
    (or dir
        (and (not nodefault)
             default-directory))))

(defcustom rustic-compile-directory-method 'rustic-buffer-crate
  "Choose function that returns the directory used when calling
 cargo commands.

If you want to use the workspace you can use `rustic-buffer-workspace'.
Note that there may exist functionality that has higher priority than
this variable."
  :type 'function
  :group 'rustic)

;;; Mode

(defvar rustic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'rustic-popup)

    (define-key map (kbd "C-c C-c C-u") 'rustic-compile)
    (define-key map (kbd "C-c C-c C-i") 'rustic-recompile)
    (define-key map (kbd "C-c C-c C-o") 'rustic-format-buffer)
    (define-key map (kbd "C-c C-c C-d") 'rustic-racer-describe)
    (define-key map (kbd "C-c C-c C-,") 'rustic-docstring-dwim)

    (define-key map (kbd "C-c C-c C-b") 'rustic-cargo-build)
    (define-key map (kbd "C-c C-c C-k") 'rustic-cargo-check)
    (define-key map (kbd "C-c C-c C-r") 'rustic-cargo-run)
    (define-key map (kbd "C-c C-c C-f") 'rustic-cargo-fmt)
    (define-key map (kbd "C-c C-c C-t") 'rustic-cargo-test)
    (define-key map (kbd "C-c C-c C-c") 'rustic-cargo-current-test)
    (define-key map (kbd "C-c C-c C-l") 'rustic-cargo-clippy)
    (define-key map (kbd "C-c C-c C-n") 'rustic-cargo-outdated)
    (define-key map (kbd "C-c C-c n") 'rustic-cargo-new)
    (define-key map (kbd "C-c C-c i") 'rustic-cargo-init)
    (define-key map (kbd "C-c C-c b") 'rustic-cargo-bench)
    (define-key map (kbd "C-c C-c d") 'rustic-cargo-doc)
    (define-key map (kbd "C-c C-c c") 'rustic-cargo-clean)
    (define-key map (kbd "C-c C-c k") 'rustic-cargo-clippy)
    (define-key map (kbd "C-c C-c f") 'rustic-cargo-clippy-fix)
    ;; cargo edit
    (define-key map (kbd "C-c C-c a") 'rustic-cargo-add)
    (define-key map (kbd "C-c C-c r") 'rustic-cargo-rm)
    (define-key map (kbd "C-c C-c u") 'rustic-cargo-upgrade)
    map)
  "Keymap for `rustic-mode'.")

;;;###autoload
(define-derived-mode rustic-mode rust-mode "Rustic"
  "Major mode for Rust code.

\\{rustic-mode-map}"
  :group 'rustic)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

;; remove rust-mode from `auto-mode-alist'
(let ((mode '("\\.rs\\'" . rust-mode)))
  (when (member mode auto-mode-alist)
    (setq auto-mode-alist (remove mode auto-mode-alist))))

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
  (require 'rustic-doc)
  (require 'rustic-clippy)
  (require 'rustic-comint)
  (require 'rustic-babel)
  (require 'rustic-racer)
  (require 'rustic-rustfmt)
  (require 'rustic-rustfix)
  (require 'rustic-playpen)
  (require 'rustic-lsp)
  (require 'rustic-expand)
  (require 'rustic-spellcheck)
  (with-eval-after-load 'flycheck
    (require 'rustic-flycheck)))

;;; rustic.el ends here
