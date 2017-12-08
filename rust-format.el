(require 'rust-mode)


(defconst rust--format-word "\\b\\(else\\|enum\\|fn\\|for\\|if\\|let\\|loop\\|match\\|struct\\|union\\|unsafe\\|while\\)\\b")
(defconst rust--format-line "\\([\n]\\)")

;; Counts number of matches of regex beginning up to max-beginning,
;; leaving the point at the beginning of the last match.
(defun rust--format-count (regex max-beginning)
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

;; Gets list describing pos or (point).
;; The list contains:
;; 1. the number of matches of rust--format-word,
;; 2. the number of matches of rust--format-line after that,
;; 3. the number of columns after that.
(defun rust--format-get-loc (buffer &optional pos)
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

;; Moves the point forward by count matches of regex up to max-pos,
;; and returns new max-pos making sure final position does not include another match.
(defun rust--format-forward (regex count max-pos)
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

;; Gets the position from a location list obtained using rust--format-get-loc.
(defun rust--format-get-pos (buffer loc)
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

(defun rust-format-buffer ()
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
        (rust-format--start-process (current-buffer))
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
          (set-window-start window start)
          (set-window-point window pos)))))

  (message "Formatted buffer with rustfmt."))

(defun rust-enable-format-on-save ()
  "Enable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rust-format-on-save t))

(defun rust-disable-format-on-save ()
  "Disable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rust-format-on-save nil))


;;;;;;;;;;;;
;; Process

(defcustom rust-format--display-method 'pop-to-buffer
  "Default function used for displaying compilation buffer."
  :type 'function)

(defvar rust-format--process-name "rust-process"
  "Process name for rust compilation processes.")

(defvar rust-format--buffer-name "*rust-fmt-buffer*"
  "Buffer name for rust compilation process buffers.")

(defun rust-format---filter (proc output)
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
       (insert (xterm-color-filter output)))))

(defun rust-format--sentinel (proc output)
  (let ((buf (process-buffer proc)))
    (if (string-match-p "^finished" output)
        (kill-buffer buf)
      (and
       (display-buffer buf)
       (goto-char (point-min))
       (next-error)))))

(defun rust-format--start-process (buf)
  (interactive)
  (let* ((tmpf (make-temp-file "rustfmt"))
         (err-buf (get-buffer-create rust-format--buffer-name))
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list (format "TERM=%s" "ansi"))
                               process-environment)))
    (with-current-buffer err-buf
      (erase-buffer)
      (rust-compilation-mode))
    (with-current-buffer buf
      (write-region (point-min) (point-max) tmpf nil t)
      (make-process :name rust-format--process-name
                    :buffer err-buf
                    :command `(,rust-rustfmt-bin ,tmpf)
                    :filter #'rust-format---filter
                    :sentinel #'rust-format--sentinel)
      (let ((proc (get-buffer-process err-buf)))
        (accept-process-output proc 0.1))
      (erase-buffer)
      (insert-file-contents tmpf nil)
      (delete-file tmpf))))

