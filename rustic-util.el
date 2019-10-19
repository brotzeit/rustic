;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Commentary:

;; Contains functions for rust tools like rustfmt and RLS.

;;; Code:

(require 'subr-x)

(require 'rustic-compile)

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rustic-format-trigger nil
  "Format future rust buffers before saving using rustfmt."
  :type '(choice (symbol :tag 'on-save "Format buffer before saving.")
                 (symbol :tag 'on-compile "Run 'cargo fmt' before compilation.")
                 (symbol :tag nil "Don't format automatically."))
  :group 'rustic)

(defcustom rustic-format-on-save nil
  "Format rust buffers before saving using rustfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)
(make-obsolete 'rustic-format-on-save 'rustic-format-trigger "0.19")

(defun rustic-format-on-save-p ()
  "Checks if either deprecated `rustic-format-on-save' or `rustic-format-trigger' is set
to format buffer when saving."
  (or rustic-format-on-save (eq rustic-format-trigger 'on-save)))

(defcustom rustic-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rustic)

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

(defcustom rustic-lsp-server 'rls
  "Choose your LSP server."
  :type '(choice (symbol :tag 'rls "rls")
                 (symbol :tag 'rust-analyzer "rust-analyzer"))
  :group 'rustic)

(defcustom rustic-lsp-client 'lsp-mode
  "Emacs package for interaction with the language server."
  :type '(choice (symbol :tag 'eglot "eglot")
                 (symbol :tag 'lsp-mode "lsp-mode")
                 (symbol :tag nil "No LSP client"))
  :group 'rustic)

(defcustom rustic-rls-pkg nil
  "Emacs package for interaction with rls."
  :type '(choice (symbol :tag 'eglot "eglot")
                 (symbol :tag 'lsp-mode "lsp-mode")
                 (symbol :tag nil "No LSP client"))
  :group 'rustic)
(make-obsolete 'rustic-rls-pkg 'rustic-lsp-client "0.18")

(defcustom rustic-lsp-format nil
  "Allow formatting through lsp server."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

(defcustom rustic-analyzer-command '("ra_lsp_server")
  "Command for calling rust analyzer."
  :type '(repeat (string)))

;;;;;;;;;;;;
;; Rustfmt

(defvar rustic-format-process-name "rustic-rustfmt-process"
  "Process name for rustfmt processes.")

(defvar rustic-format-buffer-name "*rustfmt*"
  "Buffer name for rustfmt process buffers.")

(defvar rustic-save-pos nil)

(defun rustic-format-start-process (sentinel &rest args)
  "Run rustfmt with ARGS.

Use `:command' when formatting files and `:stdin' for strings."
  (let* ((err-buf (get-buffer-create rustic-format-buffer-name))
         (inhibit-read-only t)
         (dir (rustic-buffer-workspace))
         (buffer (plist-get args :buffer))
         (string (plist-get args :stdin))
         (command (plist-get args :command)))
    (setq rustic-save-pos (point))
    (rustic-compilation-setup-buffer err-buf dir 'rustic-format-mode t)
    (when command
      (let ((file (nth 1 command)))
        (unless (file-exists-p file)
          (error (format "File %s does not exist." file)))))
    (with-current-buffer err-buf
      (let ((proc (rustic-make-process :name rustic-format-process-name
                                       :buffer err-buf
                                       :command (or command `(,rustic-rustfmt-bin))
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
            (let ((file-buffer next-error-last-buffer))
              (copy-to-buffer file-buffer (point-min) (point-max))
              (with-current-buffer file-buffer
                (goto-char rustic-save-pos))
              (kill-buffer proc-buffer)
              (message "Formatted buffer with rustfmt."))
          (goto-char (point-min))
          (when-let ((file (buffer-file-name next-error-last-buffer)))
            (save-excursion
              (save-match-data
                (when (search-forward "<stdin>" nil t)
                  (replace-match file)))))
          (funcall rustic-format-display-method proc-buffer)
          (message "Rustfmt error."))))))

(defun rustic-format-file-sentinel (proc output)
  "Sentinel for rustfmt processes when formatting a file."
  (ignore-errors
    (let ((proc-buffer (process-buffer proc)))
      (with-current-buffer proc-buffer
        (if (string-match-p "^finished" output)
            (progn
              (with-current-buffer next-error-last-buffer
                (revert-buffer t t)))
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
                        :no-display t
                        :buffer buffer
                        :process proc
                        :mode mode
                        :sentinel #'rustic-cargo-fmt-sentinel)))

(defun rustic-cargo-fmt-sentinel (proc output)
  "Sentinel for formatting with `rustic-cargo-fmt'."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer proc-buffer
      (if (not (string-match-p "^finished" output))
          (funcall rustic-compile-display-method proc-buffer)
        (kill-buffer proc-buffer)
        (message "Workspace formatted with cargo-fmt.")))))

(defun rustic-format-buffer (&optional no-stdin)
  "Format the current buffer using rustfmt.

Provide optional argument NO-STDIN for `rustic-before-save-hook' since there
were issues when using stdin for formatting."
  (interactive)
  (unless (eq major-mode 'rustic-mode)
    (error "Not a rustic-mode buffer."))
  (rustic-compilation-process-live t)
  (let (proc)
    (if (not no-stdin)
        (setq proc (rustic-format-start-process 'rustic-format-sentinel
                                                :buffer (current-buffer)
                                                :stdin (buffer-string)))
      (let* ((buf (current-buffer))
             (file (buffer-file-name buf))
             (string (buffer-string)))
        (write-region string nil file nil 0)
        (let ((command `(,rustic-rustfmt-bin ,file)))
          (setq proc (rustic-format-start-process 'rustic-format-file-sentinel
                                                  :buffer buf
                                                  :command command)))))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))))


;;;;;;;;
;; LSP

(defun rustic-setup-eglot ()
  "Configure eglot for rustic."
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

(defun rustic-setup-rls ()
  "Start the rls client's process.
If client isn't installed, offer to install it."
  (unless noninteractive ;; TODO: fix tests to work with eglot/lsp-mode activated
    (let ((client-p (lambda (c)
                      (if (package-installed-p c)
                          (or (featurep c) (require c))
                        nil)))
          (client (or rustic-rls-pkg rustic-lsp-client)))
      (cond ((eq client nil)
             nil)
            ((not (eq (funcall client-p client) nil))
             (if (eq client 'eglot)
                 (eglot-ensure)
               (lsp-workspace-folders-add (rustic-buffer-workspace))
               (setq lsp-rust-server rustic-lsp-server)
               (lsp)))
            (t
             (rustic-install-rls-client-p client))))))

(defun rustic-install-rls-client-p (rls-pkg)
  (if (yes-or-no-p (format "%s not found. Install it ?" rls-pkg))
      (condition-case err
          (progn
            (package-refresh-contents)
            (package-install rls-pkg)
            (require rls-pkg)
            (rustic-setup-rls))
        (error err))
    (message "No RLS server running.")))


;;;;;;;;;;;
;; Rustix

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
         (buf (current-buffer))
         (proc rustic-rustfix-process-name)
         (mode 'rustic-rustfix-mode))
    (rustic-compilation-process-live)
    (rustic-compilation-start command :buffer err-buf :process proc :mode mode)))


;;;;;;;;;;;;;;;;
;; Interactive

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
           (escaped-playpen-url (url-hexify-string (format rustic-playpen-url-format escaped-data))))
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

(provide 'rustic-util)
;;; rustic-util.el ends here
