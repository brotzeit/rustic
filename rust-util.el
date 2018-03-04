;;; rust-util.el --- Rust utility functions -*-lexical-binding: t-*-

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Code:

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rust-format-on-save nil
  "Format future rust buffers before saving using rustfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'rust-mode)

(defcustom rust-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rust-mode)

(defcustom rust-cargo-bin "cargo"
  "Path to cargo executable.")

(defcustom rust-format-display-method 'pop-to-buffer
  "Default function used for displaying rustfmt buffer."
  :type 'function)


;;;;;;;;;;;;
;; Rustfmt 

(defvar rust-format-process-name "rust-rustfmt-process"
  "Process name for rustfmt processes.")

(defvar rust-format-buffer-name "*rustfmt*"
  "Buffer name for rustfmt process buffers.")

(defvar rust-format-file-name nil
  "Holds last file formatted by `rust-format-start-process'.")

(defvar rust-save-pos nil)

(defun rust-format-filter (proc output)
  "Filter for rustfmt processes."
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (xterm-color-filter output)))))

(defun rust-format-sentinel (proc output)
  "Sentinel for rustfmt processes."
  (let ((proc-buffer (process-buffer proc)))
    (with-current-buffer proc-buffer
      (if (string-match-p "^finished" output)
          (let ((file-buffer (get-file-buffer rust-format-file-name)))
            (copy-to-buffer file-buffer (point-min) (point-max))
            (with-current-buffer file-buffer
              (goto-char rust-save-pos))
            (kill-buffer proc-buffer)
            (message "Formatted buffer with rustfmt."))
        (goto-char (point-min))
        (save-excursion
          (when (search-forward "<stdin>")
            (replace-match rust-format-file-name))
          (funcall rust-format-display-method proc-buffer)
          (message "Rustfmt error."))
        (compilation-next-error 1)))))

(defun rust-format-start-process (buffer string)
  "Start a new rustfmt process."
  (let* ((file (buffer-file-name buffer))
         (err-buf (get-buffer-create rust-format-buffer-name))
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list (format "TERM=%s" "ansi"))
                               process-environment)))
    (with-current-buffer err-buf
      (erase-buffer)
      (rust-format-mode))
    (setq rust-format-file-name (buffer-file-name buffer))
    (setq rust-save-pos (point))
    (let ((proc (make-process :name rust-format-process-name
                              :buffer err-buf
                              :command `(,rust-rustfmt-bin)
                              :filter #'rust-format-filter
                              :sentinel #'rust-format-sentinel)))
      (while (not (process-live-p proc))
        (sleep-for 0.01))
      (process-send-string proc string)
      (process-send-eof proc))))

(define-derived-mode rust-format-mode rust-compilation-mode "rustfmt"
  :group 'rust-mode)

;;;###autoload
(defun rust-format--enable-format-on-save ()
  "Enable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rust-format-on-save t))

;;;###autoload
(defun rust-format--disable-format-on-save ()
  "Disable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rust-format-on-save nil))

;;;###autoload
(defun rust-cargo-fmt ()
  (interactive)
  (let ((buffer-name rust-format-buffer-name)
        (mode 'rust-format-mode)
        (dir (rust-buffer-project)))
    (rust-compilation-process-live (get-process buffer-name))
    (rust-compilation-run "cargo fmt" buffer-name mode dir)))

(defun rust-format-buffer ()
  "Format the current buffer using rustfmt."
  (interactive)
  (unless (executable-find rust-rustfmt-bin)
    (error "Could not locate executable \"%s\"" rust-rustfmt-bin))
  (rust-format-start-process (current-buffer) (buffer-string)))


;;;;;;;;;;;
;; Clippy

(defvar rust-clippy-buffer-name "*rust-clippy*"
  "Buffer name for clippy buffers.")

(define-derived-mode rust-clippy-mode rust-compilation-mode "rust clippy"
  :group 'rust-mode)

;;;###autoload
(defun rust-run-clippy ()
  "Run `cargo clippy'."
  (interactive)
  (let ((command (concat rust-cargo-bin " clippy"))
        (buffer-name rust-clippy-buffer-name)
        (mode 'rust-clippy-mode)
        (root (rust-buffer-project)))
    (rust-compilation-process-live (get-process buffer-name))
    (rust-compilation-run command buffer-name mode root)))


;;;;;;;;;;;;;;;;
;; Interactive

(defun rust-playpen-region (begin end)
  "Create a sharable URL for the contents of the current region
   on the Rust playpen."
  (interactive "r")
  (let* ((data (buffer-substring begin end))
         (escaped-data (url-hexify-string data))
         (escaped-playpen-url (url-hexify-string (format rust-playpen-url-format escaped-data))))
    (if (> (length escaped-playpen-url) 5000)
        (error "encoded playpen data exceeds 5000 character limit (length %s)"
               (length escaped-playpen-url))
      (let ((shortener-url (format rust-shortener-url-format escaped-playpen-url))
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
                            (message "%s" last-line)))))))))

(defun rust-playpen-buffer ()
  "Create a sharable URL for the contents of the current buffer
   on the Rust playpen."
  (interactive)
  (rust-playpen-region (point-min) (point-max)))

;;;###autoload
(defun rust-cargo-build ()
  (interactive)
  (call-interactively 'rust-compile "cargo build"))

(define-derived-mode rust-test-mode rust-compilation-mode "rust test"
  :group 'rust-mode)

;;;###autoload
(defun rust-cargo-test ()
  (interactive)
  (call-interactively 'rust-compile "cargo test"))

(provide 'rust-util)
;;; rust-util.el ends here
