;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Commentary:

;; Contains functions for rust tools like rustfmt and RLS.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'package)
(require 'org-element)

(require 'rustic-common)
(require 'rustic-cargo)

;;; Customization

(defcustom rustic-format-display-method 'pop-to-buffer
  "Default function used for displaying rustfmt buffer."
  :type 'function
  :group 'rustic)

(defcustom rustic-playpen-url-format "https://play.rust-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen."
  :type 'string
  :group 'rustic)

(defcustom rustic-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission."
  :type 'string
  :group 'rustic)

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

;;; Rustfmt

(defvar rustic-format-process-name "rustic-rustfmt-process"
  "Process name for rustfmt processes.")

(defvar rustic-format-buffer-name "*rustfmt*"
  "Buffer name for rustfmt process buffers.")

(defvar rustic-save-pos nil
  "Marker, holding location of the cursor's position before
running rustfmt.")

(defun rustic-format-start-process (sentinel &rest args)
  "Run rustfmt with ARGS.

:buffer BUFFER -- BUFFER is the buffer that is being formatted.

:stdin STRING -- STRING will be written to the standard input of rustfmt.
When `:files' is non-nil, STRING will be ignored by rustfmt.

:files FILES -- FILES is a string or list of strings that
specify the input file or files to rustfmt.

:command COMMAND -- COMMAND is a string or a list of strings.
When COMMAND is non-nil, it replaces the default command.
When COMMAND is a string, it is the program file name.
When COMMAND is a list, it's `car' is the program file name
and it's `cdr' is a list of arguments."
  (let* ((err-buf (get-buffer-create rustic-format-buffer-name))
         (inhibit-read-only t)
         (dir (rustic-buffer-workspace))
         (buffer (plist-get args :buffer))
         (string (plist-get args :stdin))
         (files  (plist-get args :files))
         (files (if (listp files) files (list files)))
         (command (or (plist-get args :command)
                      (cons rustic-rustfmt-bin (rustic-compute-rustfmt-args))))
         (command (if (listp command) command (list command))))
    (setq rustic-save-pos (set-marker (make-marker) (point) (current-buffer)))
    (rustic-compilation-setup-buffer err-buf dir 'rustic-format-mode t)
    (--each files
      (unless (file-exists-p it)
        (error (format "File %s does not exist." it))))
    (with-current-buffer err-buf
      (let ((proc (rustic-make-process :name rustic-format-process-name
                                       :buffer err-buf
                                       :command `(,@command "--" ,@files)
                                       :filter #'rustic-compilation-filter
                                       :sentinel sentinel)))
        (setq next-error-last-buffer buffer)
        (when string
          (while (not (process-live-p proc))
            (sleep-for 0.01))
          (process-send-string proc (concat string "\n"))
          (process-send-eof proc))
        proc))))

(defun rustic-format-sentinel (proc output)
  "Sentinel for rustfmt processes when using stdin."
  (ignore-errors
    (let ((proc-buffer (process-buffer proc))
          (inhibit-read-only t))
      (with-current-buffer proc-buffer
        (if (string-match-p "^finished" output)
            (let ((file-buffer next-error-last-buffer)
                  ;; replace-buffer-contents was in emacs 26.1, but it
                  ;; was broken for non-ASCII strings, so we need 26.2.
                  (use-replace (version<= "26.2" emacs-version)))
              (unless use-replace
                (copy-to-buffer file-buffer (point-min) (point-max)))
              (with-current-buffer file-buffer
                (if use-replace
                    (replace-buffer-contents proc-buffer))
                (goto-char rustic-save-pos))
              (kill-buffer proc-buffer)
              (message "Formatted buffer with rustfmt."))
          (goto-char (point-min))
          (when-let ((file (buffer-file-name next-error-last-buffer)))
            (save-excursion
              (save-match-data
                (when (search-forward "<stdin>" nil t)
                  (replace-match file)))))
          (with-current-buffer next-error-last-buffer
            (goto-char rustic-save-pos))
          (funcall rustic-format-display-method proc-buffer)
          (message "Rustfmt error."))))))

(defun rustic-format-file-sentinel (proc output)
  "Sentinel for rustfmt processes when formatting a file."
  (ignore-errors
    (let ((proc-buffer (process-buffer proc)))
      (with-current-buffer proc-buffer
        (if (string-match-p "^finished" output)
            (with-current-buffer next-error-last-buffer
              (revert-buffer t t))
          (sit-for 0.1)
          (with-current-buffer next-error-last-buffer
            (goto-char rustic-save-pos))
          (goto-char (point-min))
          (funcall rustic-format-display-method proc-buffer)
          (message "Rustfmt error."))))))

(define-derived-mode rustic-format-mode rustic-compilation-mode "rustfmt"
  :group 'rustic)

(define-derived-mode rustic-cargo-fmt-mode rustic-compilation-mode "cargo-fmt"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-fmt ()
  "Use rustfmt via cargo."
  (interactive)
  (let ((command (list rustic-cargo-bin "fmt"))
        (buffer rustic-format-buffer-name)
        (proc rustic-format-process-name)
        (mode 'rustic-cargo-fmt-mode))
    (rustic-compilation-process-live)
    (rustic-compilation command
                        (list
                         :no-display t
                         :buffer buffer
                         :process proc
                         :mode mode
                         :sentinel #'rustic-cargo-fmt-sentinel))))

(defun rustic-cargo-fmt-sentinel (proc output)
  "Sentinel for formatting with `rustic-cargo-fmt'."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer proc-buffer
      (if (not (string-match-p "^finished" output))
          (funcall rustic-compile-display-method proc-buffer)
        (when (fboundp rustic-list-project-buffers-function)
          (let ((buffers (cl-remove-if-not
                          #'buffer-file-name
                          (funcall rustic-list-project-buffers-function))))
            (dolist (b buffers)
              (with-current-buffer b
                (revert-buffer t t)))))
        (kill-buffer proc-buffer)
        (message "Workspace formatted with cargo-fmt.")))))

;;;###autoload
(defun rustic-format-buffer ()
  "Format the current buffer using rustfmt.

Provide optional argument NO-STDIN for `rustic-before-save-hook' since there
were issues when using stdin for formatting."
  (interactive)
  (unless (or (eq major-mode 'rustic-mode)
              (eq major-mode 'rustic-macro-expansion-mode))
    (error "Not a rustic-mode buffer."))
  (rustic-compilation-process-live t)
  (rustic-format-start-process 'rustic-format-sentinel
                               :buffer (current-buffer)
                               :stdin (buffer-string)))

;;;###autoload
(defun rustic-format-file (&optional file)
  "Unlike `rustic-format-buffer' format file directly and revert the buffer."
  (interactive "P")
  (let* ((buf (current-buffer))
         (file (or (if file (read-from-minibuffer "Format file: ") nil)
                   (buffer-file-name buf)
                   (read-from-minibuffer "Format file: ")))
         (string (buffer-string)))
    (write-region string nil file nil 0)
    (let ((proc (rustic-format-start-process 'rustic-format-file-sentinel
                                             :buffer buf
                                             :files file)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.05)))))

(defun rustic-project-buffer-list ()
  "Return a list of the buffers belonging to the current project.
This is basically a wrapper around `project--buffer-list'."
  (when-let ((pr (project-current)))
    (if (fboundp 'project--buffer-list)
        (project--buffer-list pr)
      ;; Like the above function but releases before Emacs 28.
      (let ((root (cdr pr))
            bufs)
        (dolist (buf (buffer-list))
          (let ((filename (or (buffer-file-name buf)
                              (buffer-local-value 'default-directory buf))))
            (when (and filename (file-in-directory-p filename root))
              (push buf bufs))))
        (nreverse bufs)))))

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

;;; Rustix

(defvar rustic-rustfix-process-name "rustic-rustfix-process"
  "Process name for rustfix processes.")

(defvar rustic-rustfix-buffer-name "*cargo-rustfix*"
  "Buffer name for rustfix buffers.")

(define-derived-mode rustic-rustfix-mode rustic-compilation-mode "rustfix"
  :group 'rustic)

;;;###autoload
(defun rustic-rustfix ()
  "Run 'cargo fix'."
  (interactive)
  (let* ((command (list rustic-cargo-bin "fix" "--allow-dirty"))
         (err-buf rustic-rustfix-buffer-name)
         (proc rustic-rustfix-process-name)
         (mode 'rustic-rustfix-mode))
    (rustic-compilation-process-live)
    (rustic-compilation-start command (list :buffer err-buf :process proc :mode mode))))

;;; Interactive

;;;###autoload
(defun rustic-playpen (begin end)
  "Create a shareable URL for the contents of the current region,
src-block or buffer on the Rust playpen."
  (interactive "r")
  (let (data)
    (cond
     ((region-active-p)
      (setq data (buffer-substring begin end)))
     ((org-in-src-block-p)
      (setq data (org-element-property :value (org-element-at-point))))
     (t
      (setq data (buffer-substring (point-min) (point-max)))))
    (let* ((escaped-data (url-hexify-string data))
           (escaped-playpen-url (url-hexify-string
                                 (format rustic-playpen-url-format
                                         escaped-data))))
      (if (> (length escaped-playpen-url) 5000)
          (error "encoded playpen data exceeds 5000 character limit (length %s)"
                 (length escaped-playpen-url))
        (let ((shortener-url (format rustic-shortener-url-format escaped-playpen-url))
              (url-request-method "POST"))
          (url-retrieve shortener-url
                        (lambda (state)
                          ;; filter out the headers etc. included at the
                          ;; start of the buffer: the relevant text
                          ;; (shortened url or error message) is exactly
                          ;; the last line.
                          (goto-char (point-max))
                          (let ((last-line (thing-at-point 'line t))
                                (err (plist-get state :error)))
                            (kill-buffer)
                            (if err
                                (error "failed to shorten playpen url: %s" last-line)
                              (let ((URL (read-from-minibuffer "Playpen URL: " last-line)))
                                (browse-url URL)))))))))))

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
