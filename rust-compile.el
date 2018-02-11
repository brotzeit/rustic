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

(defun rust-next-error-hook ()
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
                  :filter #'rust-compile--filter
                  :sentinel #'(lambda (_proc _output)))))

(defun rust-compile--filter (proc output)
  "Filter for rust compilation process."
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
       (insert (xterm-color-filter output)))))


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

