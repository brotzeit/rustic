;;; rustic-lsp.el --- Support for lsp                -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements support for "lsp".

;;; Code:

(require 'rustic-rustfmt)

;;; Options

;; FIXME This is non-idomatic.  This option should be replaced with
;; documentation that instructs the user to add the setup function
;; themselves, iff so desired.
(defcustom rustic-lsp-setup-p t
  "Setup LSP related stuff automatically.
If this is non-nil (the default), then loading `rustic-lsp' adds
`rustic-setup-lsp' to `rustic-mode-hook'.  If you don't want that
then you must set this to nil before loading `rustic-lsp'."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)
(when rustic-lsp-setup-p
  (add-hook 'rustic-mode-hook 'rustic-setup-lsp))

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

(defcustom rustic-analyzer-command '("rust-analyzer")
  "Command for calling rust analyzer."
  :type '(repeat (string))
  :group 'rustic)

(defcustom rustic-enable-detached-file-support nil
  "(Experimental) Enable rust-analyzer's detached file support.

If enabled, rust-analyzer will try to manage a single file
without requiring a dedicate Cargo project.

Currently, only eglot is supported.  Note that due to a current
limitation, the whole directory to which the source file belongs
will be managed.  Hence, you should avoid visiting a Rust file
in, e.g. your home directory."
  :type 'boolean
  :group 'rustic)

;;; Common

(defun rustic-setup-lsp ()
  "Setup LSP client. If client isn't installed, offer to install it."
  (let ((client rustic-lsp-client))
    (cond ((eq client nil)
           nil)
          ((require client nil t)
           (if (eq client 'eglot)
               (eglot-ensure)
             (rustic-lsp-mode-setup)
             (lsp-deferred)))
          (t
           (rustic-install-lsp-client-p client)))))

;;; lsp support

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
  ;; TODO: Do we still need this ? Seems to break stuff (hlissner/doom-emacs/issues/4070)
  ;; (lsp-workspace-folders-add (rustic-buffer-workspace))
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

;;; eglot support

(defvar eglot-ignored-server-capabilites)
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
    (add-to-list 'eglot-server-programs `((rustic-mode :language-id "rust") . (eglot-rust-analyzer . ,rustic-analyzer-command)))))

(with-eval-after-load 'eglot
  (defclass eglot-rust-analyzer (eglot-lsp-server) ()
    :documentation "Rust-analyzer LSP server.")

  (cl-defmethod eglot-initialization-options ((server eglot-rust-analyzer))
    "Pass `detachedFiles' when `rustic-enable-detached-file-support' is non-`nil'."
    (if (or (null rustic-enable-detached-file-support)
            (null buffer-file-name)
            (rustic-buffer-crate t))
        eglot--{}
      (list :detachedFiles
            (vector (file-local-name (file-truename buffer-file-name))))))

  (rustic-setup-eglot))

;;; rustic-macro-expansion-mode

(setq lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand)

(define-derived-mode rustic-macro-expansion-mode special-mode "Rust"
  :group 'rustic

  :syntax-table rust-mode-syntax-table

  ;; Syntax
  (setq-local syntax-propertize-function #'rust-syntax-propertize)

  ;; Indentation
  (setq-local indent-line-function 'rust-mode-indent-line)

  ;; Fonts
  (setq-local font-lock-defaults
              '(rust-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . rust-mode-syntactic-face-function)))
  )

;;;###autoload
(defun rustic-analyzer-macro-expand (result)
  "Default method for displaying macro expansion RESULT ."
  (rustic--inheritenv
   (let* ((root (lsp-workspace-root default-directory))
          (buf (get-buffer-create
                (format "*rust-analyzer macro expansion %s*" root))))
     (with-current-buffer buf
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert result)
         (rustic-macro-expansion-mode)))
     (display-buffer buf))))

;;; _
(provide 'rustic-lsp)
;;; rustic-lsp.el ends here
