;;; rust-compile.el --- Compile facilities for rust-mode -*-lexical-binding: t-*-

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Code:

(require 'xterm-color)
(require 'compile)

;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup rust-compilation nil
  "Rust Compilation."
  :group 'processes)

(defcustom rust-compile-command (purecopy "cargo build")
  "Default command for rust compilation."
  :type 'string
  :group 'rust-compilation)

(defcustom rust-compile-display-method 'display-buffer
  "Default function used for displaying compilation buffer."
  :type 'function
  :group 'rust-compile)

;; Faces

(defcustom rust-message-face
  '((t :inherit default))
  "Don't use `compilation-message-face', as ansi colors get messed up."
  :type 'face
  :group 'rust-compilation)

(defcustom rust-ansi-faces ["black"
                            "red3"
                            "green3"
                            "yellow3"
                            "blue2"
                            "magenta3"
                            "cyan3"
                            "white"]
  "Term ansi faces."
  :type '(vector string string string string string string string string)
  :group 'rust-compilation)


;;;;;;;;;;;;;;;;;;;;;
;; Compilation-mode

(defvar rust-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map compilation-mode-map)
    (define-key map "g" 'rust-recompile)
    map)
  "Keymap for rust compilation log buffers.")

(define-compilation-mode rust-compilation-mode "rust-compilation"
  "Rust compilation mode."
  (setq-local compilation-message-face rust-message-face)
  (setq-local xterm-color-names-bright rust-ansi-faces)
  (setq-local xterm-color-names rust-ansi-faces)
  (add-hook 'next-error-hook 'rustc-scroll-down-after-next-error)

  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rust-arrow rust-compilation-regexps-arrow))
    (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rust-colon rust-compilation-regexps-colon))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'rust-arrow)
  (add-to-list 'compilation-error-regexp-alist 'rust-colon))

(defvar rust-compilation-directory nil
  "Directory to restore to when doing `rust-recompile'.")

(defvar rust-compilation-regexps-arrow
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for file paths containing '-->'.")

(defvar rust-compilation-regexps-colon
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *::: " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3 0)))) ;; 0 for info type
  "Create hyperlink in compilation buffers for file paths containing ':::'.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar rust-cargo-compilation-regexps
  '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
See `compilation-error-regexp-alist' for help on their format.")


;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation Process

(defvar rust-compile-process-name "rust-comilation-process"
  "Process name for rust compilation processes.")

(defvar rust-compile-buffer-name "*rust-compilation*"
  "Buffer name for rust compilation process buffers.")

(defvar rust-compilation-arguments nil
  "Arguments that were given to `rust-compile'")

(defun rust-compile-start-process (command buffer process mode directory &optional sentinel)
  (let ((buf (get-buffer-create buffer))
        (default-directory directory)
        (coding-system-for-read 'binary)
        (process-environment (nconc
	                          (list (format "TERM=%s" "ansi"))
                              process-environment))
        (inhibit-read-only t))
    (setq next-error-last-buffer buf)
    (with-current-buffer buf
      (setq-local default-directory directory)
      (erase-buffer)
      (funcall mode)
      (funcall rust-compile-display-method buf))
    (make-process :name process
                  :buffer buf
                  :command command
                  :filter #'rust-compile-filter
                  :sentinel (if sentinel sentinel #'(lambda (proc output))))))

(defun rust-compile-filter (proc string)
  "Insert the text emitted by PROC.
Translate STRING with `xterm-color-filter'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            ;; `save-excursion' doesn't use the right insertion-type for us.
            (pos (copy-marker (point) t))
            ;; `save-restriction' doesn't use the right insertion type either:
            ;; If we are inserting at the end of the accessible part of the
            ;; buffer, keep the inserted text visible.
	        (min (point-min-marker))
	        (max (copy-marker (point-max) t))
	        (compilation-filter-start (marker-position (process-mark proc)))
            (xterm-string (xterm-color-filter string)))
        (unwind-protect
            (progn
	          (widen)
	          (goto-char compilation-filter-start)
              ;; We used to use `insert-before-markers', so that windows with
              ;; point at `process-mark' scroll along with the output, but we
              ;; now use window-point-insertion-type instead.

              (insert xterm-string)

              (unless comint-inhibit-carriage-motion
                (comint-carriage-motion (process-mark proc) (point)))
              (set-marker (process-mark proc) (point))
              (run-hooks 'compilation-filter-hook))
	      (goto-char pos)
          (narrow-to-region min max)
	      (set-marker pos nil)
	      (set-marker min nil)
	      (set-marker max nil))))))

(defun rustc-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
   matches on the file name (which appears after `-->`), but the
   start of the error appears a few lines earlier. This hook runs
   after `M-x next-error`; it simply scrolls down a few lines in
   the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'rust-mode)
      (select-window (get-buffer-window next-error-last-buffer 'visible))
      (when (save-excursion
              (beginning-of-line)
              (looking-at " *-->"))
        (let ((start-of-error
               (save-excursion
                 (beginning-of-line)
                 (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                   (forward-line -1))
                 (point))))
          (set-window-start (selected-window) start-of-error))))))


;;;;;;;;;;;;;;;;
;; Interactive

(defun rust-compilation-process-live ()
  "Check if there's already a running rust process. 

Don't allow two rust processes at once."
  (dolist (p-name (list rust-compile-process-name
                        rust-format-process-name
                        rust-clippy-process-name
                        rust-test-process-name))
    (let ((proc (get-process p-name)))
      (when (process-live-p proc)
        (if (yes-or-no-p
             (format "`%s' is running; kill it? " p-name))
            (condition-case ()
                (progn
                  (interrupt-process proc)
                  (sit-for 0.5)
                  (delete-process proc))
              (error nil))
          (error "Cannot have two rust processes at once.")))))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate))

;;;###autoload
(defun rust-compile (&optional arg)
  "Compile rust project. If called without arguments use `rust-compile-command'.

Otherwise use provided arguments and store them in `rust-compilation-arguments'."
  (interactive "P")
  (let* ((command (if arg
                      (setq rust-compilation-arguments
                            (read-from-minibuffer "Compile command: "))
                    rust-compile-command))
         (buffer-name rust-compile-buffer-name)
         (proc-name rust-compile-process-name)
         (mode 'rust-compilation-mode)
         (dir (setq rust-compilation-directory (rust-buffer-workspace))))
    (rust-compilation-process-live)
    (rust-compile-start-process (split-string command) buffer-name proc-name mode dir)))

;;;###autoload
(defun rust-recompile ()
  "Re-compile the program using the last `rust-compile' arguments."
  (interactive)
  (let* ((command (if (not rust-compilation-arguments)
                     rust-compile-command
                   rust-compilation-arguments))
        (buffer-name rust-compile-buffer-name)
        (proc-name rust-compile-process-name)
        (mode 'rust-compilation-mode)
        (dir (or rust-compilation-directory default-directory)))
    (rust-compilation-process-live)
    (rust-compile-start-process (split-string command) buffer-name proc-name mode dir)))

(provide 'rust-compile)
;;; rust-compile.el ends here
