;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Commentary:

;; Contains functions for rust tools like rustfmt and RLS.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'package)

(require 'rustic-common)
(require 'rustic-cargo)

;;; Customization

(defcustom rustic-lsp-server 'rust-analyzer
  "Choose your LSP server."
  :type '(choice (const :tag "rls" rls)
                 (const :tag "rust-analyzer" rust-analyzer))
  :group 'rustic)

(define-obsolete-variable-alias 'rustic-rls-pkg 'rustic-lsp-client "Rustic 0.18")
(defcustom rustic-lsp-client 'lsp-mode
  "Emacs package for interaction with the language server."
  :type '(choice (const :tag "eglot" eglot)
                 (const :tag "lsp-mode" lsp-mode)
                 (const :tag "No LSP client" nil))
  :group 'rustic)

(defcustom rustic-lsp-format nil
  "Allow formatting through lsp server."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

(defcustom rustic-analyzer-command '("rust-analyzer")
  "Command for calling rust analyzer."
  :type '(repeat (string))
  :group 'rustic)

(defcustom rustic-list-project-buffers-function
  (if (fboundp 'projectile-project-buffers)
      'projectile-project-buffers
    'rustic-project-buffer-list)
  "Function used to list buffers belonging to current project."
  :type '(choice (const projectile-project-buffers)
                 (const rustic-project-buffer-list)
                 function)
  :group 'rustic)

;;; LSP

(defun rustic-setup-lsp ()
  "Setup LSP client. If client isn't installed, offer to install it."
  (let ((client rustic-lsp-client))
    (cond ((eq client nil)
           nil)
          ((require client nil t)
           (if (eq client 'eglot)
               (eglot-ensure)
             (rustic-lsp-mode-setup)
             (lsp)))
          (t
           (rustic-install-lsp-client-p client)))))

;;;; lsp

(defvar lsp-rust-analyzer-macro-expansion-method)
(defvar lsp-rust-analyzer-server-command)
(defvar lsp-rust-server)
(declare-function lsp "lsp-mode" (&optional arg))
(declare-function lsp-rust-switch-server "lsp-rust" (lsp-server))
(declare-function lsp-workspace-folders-add "lsp-rust" (project-root))
(declare-function lsp-workspace-root "lsp-mode" (&optional path))

(defun rustic-lsp-mode-setup ()
  "When changing the `lsp-rust-server', it's also necessary to update the priorities
with `lsp-rust-switch-server'."
  (require 'lsp-rust)
  (require 'lsp-modeline)
  (lsp-workspace-folders-add (rustic-buffer-workspace))
  (setq lsp-rust-server rustic-lsp-server)
  (setq lsp-rust-analyzer-server-command rustic-analyzer-command)
  (lsp-rust-switch-server rustic-lsp-server))

(defun rustic-install-lsp-client-p (lsp-client)
  "Ask user whether to install missing LSP-CLIENT."
  (if (yes-or-no-p (format "%s not found. Install it ?" lsp-client))
      (condition-case err
          (progn
            (package-refresh-contents)
            (package-install lsp-client)
            (require lsp-client)
            (rustic-setup-lsp))
        (error err))
    (message "No LSP server running.")))

;;;; eglot

(defvar eglot-ignored-server-capabilites)
(defvar eglot-ignored-server-capabilites)
(defvar eglot-server-programs)
(defvar eglot-server-programs)
(declare-function eglot-ensure "eglot" ())

(defun rustic-setup-eglot ()
  "Configure eglot for rustic."
  (require 'eglot)
  (if (equal rustic-lsp-server 'rls)
      ;; add rustic to `eglot-server-programs'
      (let ((rls '(rustic-mode . (eglot-rls "rls"))))
        (unless (member rls eglot-server-programs)
          (setq eglot-server-programs
                `(,rls
                  ;; replace rust-mode with rustic
                  ,@(-remove-first (lambda (mode)
                                     (when (symbolp (car mode))
                                       (eq (car mode) 'rust-mode)))
                                   eglot-server-programs)))))
    (add-to-list 'eglot-server-programs `(rustic-mode . ,rustic-analyzer-command)))
  ;; don't allow formatting with rls
  (unless rustic-lsp-format
    (let ((feature :documentFormattingProvider))
      (unless (-contains? eglot-ignored-server-capabilites feature)
        (add-to-list 'eglot-ignored-server-capabilites feature)))))

;;;; lsp-mode

(setq lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand)

(define-derived-mode rustic-macro-expansion-mode special-mode "Rust"
  :group 'rustic
  :syntax-table rustic-mode-syntax-table
  ;; Fonts
  (setq-local font-lock-defaults '(rustic-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function
                                    . rustic-syntactic-face-function))))

;;;###autoload
(defun rustic-analyzer-macro-expand (result)
  "Default method for displaying macro expansion results."
  (interactive)
  (let* ((root (lsp-workspace-root default-directory))
         (buf (get-buffer-create
               (format "*rust-analyzer macro expansion %s*" root))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; wrap expanded macro in a main function so we can run rustfmt
        (insert "fn main()")
        ;; rustfmt complains about $s
        (insert (replace-regexp-in-string "\\$" "" result))
        (rustic-macro-expansion-mode)
        (rustic-format-buffer)
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (delete-region (point-min) (line-end-position))
            (goto-char (point-max))
            (forward-line -1)
            (delete-region (line-beginning-position) (point-max))))))
    (display-buffer buf)))

;;; Interactive

;;;###autoload
(defun rustic-open-dependency-file ()
  "Open the 'Cargo.toml' file at the project root if the current buffer is
visiting a project."
  (interactive)
  (let ((workspace (rustic-buffer-workspace t)))
    (if workspace
        (find-file (concat workspace "/Cargo.toml"))
      (message "The current buffer is not inside a rust project!"))))

;;; _
(provide 'rustic-util)
;;; rustic-util.el ends here
