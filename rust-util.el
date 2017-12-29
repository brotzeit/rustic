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

(defcustom rust-next-error-after-format t
  "Automatically jump to first error after using `rust-format-call'."
  :type 'bool
  :group 'rust-mode)


(defconst rust--format-word "\\b\\(else\\|enum\\|fn\\|for\\|if\\|let\\|loop\\|match\\|struct\\|union\\|unsafe\\|while\\)\\b")
(defconst rust--format-line "\\([\n]\\)")

(defun rust-buffer-project ()
  "Get project root if possible."
  (with-temp-buffer
    (let ((ret (call-process rust-cargo-bin nil t nil "locate-project")))
      (when (/= ret 0)
        (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
      (goto-char 0)
      (let ((output (json-read)))
        (cdr (assoc-string "root" output))))))

(defun rust--format-count (regex max-beginning)
  "Counts number of matches of regex beginning up to max-beginning,
 leaving the point at the beginning of the last match."
  (let ((count 0)
        save-point
        beginning)
    (while (and (< (point) max-beginning)
                (re-search-forward regex max-beginning t))
      (setq count (1+ count))
      (setq beginning (match-beginning 1)))
    ;; try one more in case max-beginning lies in the middle of a match
    (setq save-point (point))
    (when (re-search-forward regex nil t)
      (let ((try-beginning (match-beginning 1)))
        (if (> try-beginning max-beginning)
            (goto-char save-point)
          (setq count (1+ count))
          (setq beginning try-beginning))))
    (when beginning (goto-char beginning))
    count))

(defun rust--format-get-loc (buffer &optional pos)
  "Gets list describing pos or (point).
 The list contains:
 1. the number of matches of rust--format-word,
 2. the number of matches of rust--format-line after that,
 3. the number of columns after that."
  (with-current-buffer buffer
    (save-excursion
      (let ((pos (or pos (point)))
            words lines columns)
        (goto-char (point-min))
        (setq words (rust--format-count rust--format-word pos))
        (setq lines (rust--format-count rust--format-line pos))
        (if (> lines 0)
            (if (= (point) pos)
                (setq columns -1)
              (forward-char 1)
              (goto-char pos)
              (setq columns (current-column)))
          (let ((initial-column (current-column)))
            (goto-char pos)
            (setq columns (- (current-column) initial-column))))
        (list words lines columns)))))

(defun rust--format-forward (regex count max-pos)
  "Moves the point forward by count matches of regex up to max-pos,
 and returns new max-pos making sure final position does not include another match."
  (when (< (point) max-pos)
    (let ((beginning (point)))
      (while (> count 0)
        (setq count (1- count))
        (re-search-forward regex nil t)
        (setq beginning (match-beginning 1)))
      (when (re-search-forward regex nil t)
        (setq max-pos (min max-pos (match-beginning 1))))
      (goto-char beginning)))
  max-pos)

(defun rust--format-get-pos (buffer loc)
  "Gets the position from a location list obtained using rust--format-get-loc."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((max-pos (point-max))
            (words (pop loc))
            (lines (pop loc))
            (columns (pop loc)))
        (setq max-pos (rust--format-forward rust--format-word words max-pos))
        (setq max-pos (rust--format-forward rust--format-line lines max-pos))
        (when (> lines 0) (forward-char))
        (let ((initial-column (current-column))
              (save-point (point)))
          (move-end-of-line nil)
          (when (> (current-column) (+ initial-column columns))
            (goto-char save-point)
            (forward-char columns)))
        (min (point) max-pos)))))


;;;;;;;;;;;;
;; Process

(defvar rust-format-process-name "rust-process"
  "Process name for rust compilation processes.")

(defvar rust-format--buffer-name "*rust-fmt-buffer*"
  "Buffer name for rust compilation process buffers.")

(defvar rustfmt-files nil
  "Holds file and tmpfile for `rust-format-filter' and `rust-format-sentinel'")

(defconst rust-tmp-file "rustfmt")

(defun rust-format-filter (proc output)
  "Filter for rustfmt processes."
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (xterm-color-filter
               (replace-regexp-in-string (cdr rustfmt-files) (car rustfmt-files) output))))))

(defun rust-format-sentinel (proc output)
  "Sentinel for rustfmt processes."
  (let ((buf (process-buffer proc))
        (tmpfile (cdr rustfmt-files)))
    (insert-file-contents tmpfile nil nil nil t)
    (delete-file tmpfile)
    (if (string-match-p "^finished" output)
        (kill-buffer buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (when rust-next-error-after-format
          (next-error))
        (funcall rust-format-display-method buf)))))

(defun rust-format-start-process (buf)
  "Start a new rustfmt process."
  (let* ((file (buffer-file-name buf))
         (temporary-file-directory rust-buffer-project-dir)
         (tmpf (make-temp-file rust-tmp-file))
         (err-buf (get-buffer-create rust-format--buffer-name))
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list (format "TERM=%s" "ansi"))
                               process-environment)))
    (setq rustfmt-files (cons file tmpf))
    (with-current-buffer err-buf
      (erase-buffer)
      (rust-compilation-mode))
    (with-current-buffer buf
      (write-region (point-min) (point-max) tmpf nil nil)
      (make-process :name rust-format-process-name
                    :buffer err-buf
                    :command `(,rust-rustfmt-bin ,tmpf)
                    :filter #'rust-format-filter
                    :sentinel #'rust-format-sentinel)
      (let ((proc (get-buffer-process err-buf)))
        (accept-process-output proc 0.1)))))


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

(defun rust-format-call ()
  "Format the current buffer using rustfmt."
  (interactive)
  (unless (executable-find rust-rustfmt-bin)
    (error "Could not locate executable \"%s\"" rust-rustfmt-bin))

  (let* ((current (current-buffer))
         (base (or (buffer-base-buffer current) current))
         buffer-loc
         window-loc)
    (dolist (buffer (buffer-list))
      (when (or (eq buffer base)
                (eq (buffer-base-buffer buffer) base))
        (push (list buffer
                    (rust--format-get-loc buffer nil))
              buffer-loc)))
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (or (eq buffer base)
                  (eq (buffer-base-buffer buffer) base))
          (let ((start (window-start window))
                (point (window-point window)))
            (push (list window
                        (rust--format-get-loc buffer start)
                        (rust--format-get-loc buffer point))
                  window-loc)))))
    (unwind-protect
        (rust-format-start-process (current-buffer))
      (dolist (loc buffer-loc)
        (let* ((buffer (pop loc))
               (pos (rust--format-get-pos buffer (pop loc))))
          (with-current-buffer buffer
            (goto-char pos))))
      (dolist (loc window-loc)
        (let* ((window (pop loc))
               (buffer (window-buffer window))
               (start (rust--format-get-pos buffer (pop loc)))
               (pos (rust--format-get-pos buffer (pop loc))))
          (unless (eq buffer current)
            (set-window-start window start))
          (set-window-point window pos)))))

  (message "Formatted buffer with rustfmt."))

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
(defun rust-run-clippy ()
  "Run `cargo clippy'."
  (when (null rust-buffer-project-dir)
  (let ((command (list rust-cargo-bin "clippy" (concat "--manifest-path=" rust-buffer-project-dir))))
    (rust-compile-start-process command))))

(provide 'rust-util)
;;; rust-util.el ends here
