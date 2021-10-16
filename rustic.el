;;; rustic.el --- Rust development environment -*-lexical-binding: t-*-

;; Version: 1.4
;; Author: Mozilla
;;
;; Keywords: languages
;; Package-Requires: ((emacs "26.1") (rust-mode "0.5.0") (dash "2.13.0") (f "0.18.2") (let-alist "1.0.4") (markdown-mode "2.3") (project "0.3.0") (s "1.10.0") (seq "2.3") (spinner "1.7.3") (xterm-color "1.6"))

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
(defalias 'rustic-beginning-of-defun #'rust-beginning-of-defun)
(defalias 'rustic-end-of-defun #'rust-end-of-defun)

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
