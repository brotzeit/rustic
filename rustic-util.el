;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Code:

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rustic-format-on-save t
  "Format future rust buffers before saving using rustfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

(defcustom rustic-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rustic)

(defcustom rustic-format-display-method 'pop-to-buffer
  "Default function used for displaying rustfmt buffer."
  :type 'function
  :group 'rustic)

(defcustom rustic-playpen-url-format "https://play.rustic-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen"
  :type 'string
  :group 'rustic)

(defcustom rustic-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission"
  :type 'string
  :group 'rustic)


;;;;;;;;;;;;
;; Rustfmt 

(defvar rustic-format-process-name "rustic-rustfmt-process"
  "Process name for rustfmt processes.")

(defvar rustic-format-buffer-name "*rustfmt*"
  "Buffer name for rustfmt process buffers.")

(defvar rustic-format-file-name nil
  "Holds last file formatted by `rustic-format-start-process'.")

(defvar rustic-save-pos nil)

(defun rustic-format-start-process (buffer sentinel &optional string command)
  "Start a new rustfmt process."
  (let* ((err-buf (get-buffer-create rustic-format-buffer-name))
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list (format "TERM=%s" "ansi"))
                               process-environment))
         (inhibit-read-only t)
         (dir (rustic-buffer-workspace)))
    (setq next-error-last-buffer buffer)
    (setq rustic-save-pos (point))
    (with-current-buffer err-buf
      (setq-local default-directory dir)
      (erase-buffer)
      (rustic-format-mode))
    (when command
      (let ((file (nth 1 command)))
        (unless (file-exists-p file)
          (error (format "File %s does not exist." file))))
      (cl-assert (and (listp command) (= (length command) 2))))
    (let ((proc (make-process :name rustic-format-process-name
                              :buffer err-buf
                              :command (if command command
                                         `(,rustic-rustfmt-bin))
                              :filter #'rustic-compilation-filter
                              :sentinel sentinel)))
      (while (not (process-live-p proc))
        (sleep-for 0.01))
      (when string 
        (process-send-string proc (concat string "\n"))
        (process-send-eof proc))
      proc)))

(defun rustic-format-sentinel (proc output)
  "Sentinel for rustfmt processes when using stdin."
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
        (message "Rustfmt error.")))))

(defun rustic-format-file-sentinel (proc output)
  "Sentinel for rustfmt processes when formatting a file."
  (let ((proc-buffer (process-buffer proc)))
    (with-current-buffer proc-buffer
      (if (string-match-p "^finished" output)
          (kill-buffer proc-buffer)
        (goto-char (point-min))
        (funcall rustic-format-display-method proc-buffer)
        (message "Rustfmt error.")))))

(define-derived-mode rustic-format-mode rustic-compilation-mode "rustfmt"
  :group 'rustic)

(define-derived-mode rustic-cargo-fmt-mode rustic-compilation-mode "cargo-fmt"
  :group 'rustic)

;;;###autoload
(defun rustic-format--enable-format-on-save ()
  "Enable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rustic-format-on-save t))

;;;###autoload
(defun rustic-format--disable-format-on-save ()
  "Disable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rustic-format-on-save nil))

;;;###autoload
(defun rustic-cargo-fmt ()
  "Use rustfmt via cargo."
  (interactive)
  (let ((command (list rustic-cargo-bin "fmt"))
        (buffer-name rustic-format-buffer-name)
        (proc-name rustic-format-process-name)
        (mode 'rustic-cargo-fmt-mode)
        (dir (rustic-buffer-workspace))
        (sentinel #'(lambda (proc output)
                      (let ((proc-buffer (process-buffer proc))
                            (inhibit-read-only t))
                        (with-current-buffer proc-buffer
                          (when (string-match-p "^finished" output)
                            (kill-buffer proc-buffer)
                            (message "Workspace formatted with cargo-fmt.")))))))
    (rustic-compilation-process-live)
    (rustic-compilation-start command buffer-name proc-name mode dir sentinel)))

(defun rustic-format-buffer ()
  "Format the current buffer using rustfmt."
  (interactive)
  (unless (eq major-mode 'rustic-mode)
    (error "Not a rustic-mode buffer."))
  (rustic-compilation-process-live t)
  (rustic-format-start-process (current-buffer) 'rustic-format-sentinel (buffer-string)))


;;;;;;;;
;; RLS

(defun rustic-setup-eglot ()
  "Configure eglot for rustic."
  ;; replace rust-mode with rustic
  (let ((rls '(rustic-mode . (eglot-rls "rls"))))
    (unless (member rls eglot-server-programs)
      (setq eglot-server-programs
            `(,rls
              ,@(-remove-first (lambda (mode)
                                 (when (symbolp (car mode))
                                   (eq (car mode) 'rust-mode)))
                               eglot-server-programs)))))
  ;; don't allow formatting with rls
  (let ((feature :documentFormattingProvider))
    (unless (-contains? eglot-ignored-server-capabilites feature)
      (add-to-list 'eglot-ignored-server-capabilites feature))))

(defun rustic-setup-rls ()
  (unless noninteractive ;; TODO: fix tests to work with eglot/lsp-mode activated
    (let ((rls-pkg rustic-rls-pkg))
      (cond ((and (eq rls-pkg 'eglot)
                  (featurep 'eglot))
             (eglot-ensure))
            ((and (eq rls-pkg 'lsp-mode)
                  (featurep 'lsp-mode))
             (lsp-rust-enable))
            ((eq rls-pkg nil)
             nil)
            (t
             (rustic-setup-rls-1 rls-pkg))))))

(defun rustic-setup-rls-1 (rls-pkg)
  (if (yes-or-no-p (format "%s not found. Install it ?" rls-pkg))
      (condition-case err
          (and (package-install rls-pkg)
               (require rls-pkg)
               (if (eq rls-pkg 'eglot)
                   (rustic-setup-eglot)
                 (require 'rustic-lsp)))
        (error err))
    (message "No RLS server running.")))

(add-hook 'rustic-mode-hook 'rustic-setup-rls)


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

(provide 'rustic-util)
;;; rustic-util.el ends here
