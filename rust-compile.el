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
  :group 'tools
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

(define-compilation-mode rust-compilation-mode "rust-compilation"
  "Rust compilation mode."
  (setq buffer-read-only nil)
  (setq-local compilation-message-face rust-message-face)
  (setq-local xterm-color-names-bright rust-ansi-faces)
  (setq-local xterm-color-names rust-ansi-faces)

  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rust-arrow rust-compilation-regexps-arrow))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'rust-arrow))

(defvar rust-compilation-directory nil
  "Directory to restore to when doing `rust-recompile'.")

;; Issue #6887: Rather than inheriting the 'gnu compilation error
;; regexp (which is broken on a few edge cases), add our own 'rust
;; compilation error regexp and use it instead.
(defvar rustc-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)")
        (end-line   "\\([0-9]+\\)")
        (end-col    "\\([0-9]+\\)")
        (msg-type   "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\|\\([Nn]ote\\|[Hh]elp\\)\\)"))
    (let ((re (concat "^" file ":" start-line ":" start-col
                      ": " end-line ":" end-col
                      " " msg-type ":")))
      (cons re '(1 (2 . 4) (3 . 5) (6 . 7)))))
  "Specifications for matching errors in rustc invocations.")

(defvar rust-compilation-regexps-arrow
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for file paths containing '-->'.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar rust-cargo-compilation-regexps
  '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
2See `compilation-error-regexp-alist' for help on their format.")

;;;;;;;;;;;;;
;; Process

(defvar rust-compile-process-name "rust-comilation-process"
  "Process name for rust compilation processes.")

(defvar rust-compile-buffer-name "*rust-compilation*"
  "Buffer name for rust compilation process buffers.")

(defvar rust-compilation-arguments nil
  "Arguments that were given to `rust-compile'")

(defun rust-compile-start-process (command buffer)
  (let ((buf (get-buffer-create buffer))
        (coding-system-for-read 'binary)
        (process-environment (nconc
	                          (list (format "TERM=%s" "ansi"))
                              process-environment)))
    (with-current-buffer buf
      (erase-buffer)
      (rust-compilation-mode)
      (funcall rust-compile-display-method buf))
    (make-process :name rust-compile-process-name
                  :buffer buf
                  :command command
                  :filter #'rust-compile-filter
                  :sentinel #'(lambda (_proc _output)))))

;; compile.el functions

(defun compile-goto-error (&optional event)
  "We don't want paths preceeded by ':::' to be treated as an error, but this function has to
be able to visit the source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (compilation--ensure-parse (point))
  (let ((string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (regexp (concat "^ *::: " "\\([^\n]+\\)" ":" "\\([0-9]+\\)" ":" "\\([0-9]+\\)")))
    (if (string-match regexp
                      string)
        (let* ((s (string-reverse (split-string string ":")))
               (file (concat rust-compilation-directory (string-trim (nth 2 s))))
               (line (nth 1 s))
               (column (nth 0 s)))
          (when  (file-exists-p file)
            (find-file-other-window file)
            (goto-line (string-to-number line))
            (move-to-column (- (string-to-number column) 1))))
      (if (get-text-property (point) 'compilation-directory)
          (dired-other-window
           (car (get-text-property (point) 'compilation-directory)))
        (setq compilation-current-error (point))
        (next-error-internal)))))

(defun rust-compile-filter (proc string)
  "This filter was copied from compile.el, but the text is inserted by xterm-color. "
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
	        (compilation-filter-start (marker-position (process-mark proc))))
        (unwind-protect
            (progn
	          (widen)
	          (goto-char compilation-filter-start)
              ;; We used to use `insert-before-markers', so that windows with
              ;; point at `process-mark' scroll along with the output, but we
              ;; now use window-point-insertion-type instead.
              
              (insert (xterm-color-filter string))
              
              (unless comint-inhibit-carriage-motion
                (comint-carriage-motion (process-mark proc) (point)))
              (set-marker (process-mark proc) (point))
              (run-hooks 'compilation-filter-hook))
	      (goto-char pos)
          (narrow-to-region min max)
	      (set-marker pos nil)
	      (set-marker min nil)
	      (set-marker max nil))))))


;;;;;;;;;;;;;;;;
;; Interactive

;;;###autoload
(defun rust-compile (&optional arg)
  "Compile rust project. If called without arguments use `rust-compile-command'.

Otherwise use provided arguments and store them in `rust-compilation-arguments'."
  (interactive "P")
  (let ((command (if arg
                     (setq rust-compilation-arguments
                           (read-from-minibuffer "Compile command: "))
                   rust-compile-command))
        (proc (get-process rust-compile-process-name))
        (default-directory (rust-buffer-project)))
    (when (process-live-p proc)
      (if (yes-or-no-p
           (format "A rust-compile process is running; kill it? "))
          (condition-case ()
              (progn
                (interrupt-process proc)
                (sit-for 1)
                (delete-process proc))
            (error nil))
        (error "Cannot have two processes in `%s' at once"
               (buffer-name))))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default rust-compilation-directory default-directory)
    (rust-compile-start-process (split-string command) rust-compile-buffer-name)))

;;;###autoload
(defun rust-recompile ()
  "Re-compile the program using the last `rust-compile' arguments."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or rust-compilation-directory default-directory))
        (command (if (not rust-compilation-arguments)
                     rust-compile-command
                   rust-compilation-arguments)))
    (rust-compile-start-process command rust-compile-buffer-name)))

(provide 'rust-compile)
;;; rust-compile.el ends here

