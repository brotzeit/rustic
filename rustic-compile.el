;;; rustic-compile.el --- Compile facilities -*-lexical-binding: t-*-

;;; Commentary:

;; Unlike compile.el, rustic makes use of a non dumb terminal in order to receive
;; all ANSI control sequences, which get translated by xterm-color.
;; This file also adds a dervied compilation mode. Error matching regexes from
;; compile.el are removed.

;;; Code:

(require 'compile)
(require 'xterm-color)
(require 'projectile)

;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup rustic-compilation nil
  "Rust Compilation."
  :group 'processes)

(defcustom rustic-compile-command (purecopy "cargo build")
  "Default command for rust compilation."
  :type 'string
  :group 'rustic-compilation)

(defcustom rustic-compile-display-method 'display-buffer
  "Default function used for displaying compilation buffer."
  :type 'function
  :group 'rustic-compilation)

(defcustom rustic-compile-backtrace "1"
  "Set process variable `RUST_BACKTRACE'."
  :type '(choice (string :tag "0")
                 (string :tag "1")
                 (string :tag "full"))
  :group 'rustic-compilation)


;; Faces

(defcustom rustic-message-face
  '((t :inherit default))
  "Don't use `compilation-message-face', as ansi colors get messed up."
  :type 'face
  :group 'rustic-compilation)

(defcustom rustic-compilation-error-face
  '((t :inherit default))
  "Override `compilation-error-face' for rust compilation."
  :type 'face
  :group 'rustic-compilation)

(defcustom rustic-compilation-info-face
  '((t :inherit default))
  "Override `compilation-info-face' for rust compilation."
  :type 'face
  :group 'rustic-compilation)

(defcustom rustic-compilation-line-face
  '((t :inherit default))
  "Override `compilation-line-face' for rust compilation."
  :type 'face
  :group 'rustic-compilation)

(defcustom rustic-compilation-column-face
  '((t :inherit default))
  "Override `compilation-column-face' for rust compilation."
  :type 'face
  :group 'rustic-compilation)

(defcustom rustic-ansi-faces ["black"
                              "red3"
                              "green3"
                              "yellow3"
                              "blue2"
                              "magenta3"
                              "cyan3"
                              "white"]
  "Term ansi faces."
  :type '(vector string string string string string string string string)
  :group 'rustic-compilation)


;;;;;;;;;;;;;;;;;;;;;
;; Compilation-mode

(defvar rustic-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map compilation-mode-map)
    (define-key map "p" 'rustic-cargo-popup)
    (define-key map "g" 'rustic-recompile)
    map)
  "Keymap for rust compilation log buffers.")

(define-compilation-mode rustic-compilation-mode "rust-compilation"
  "Rust compilation mode.

Error matching regexes from compile.el are removed."
  (setq-local compilation-message-face rustic-message-face)
  (setq-local compilation-error-face rustic-compilation-error-face)
  (setq-local compilation-info-face rustic-compilation-info-face)
  (setq-local compilation-column-face rustic-compilation-line-face)
  (setq-local compilation-line-face rustic-compilation-column-face)
  
  (setq-local xterm-color-names-bright rustic-ansi-faces)
  (setq-local xterm-color-names rustic-ansi-faces)
  (add-hook 'next-error-hook 'rustc-scroll-down-after-next-error)

  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-error rustic-compilation-error))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-info rustic-compilation-info))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-panic rustic-compilation-panic))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'rustic-error)
  (add-to-list 'compilation-error-regexp-alist 'rustic-info)
  (add-to-list 'compilation-error-regexp-alist 'rustic-panic)

  (add-hook 'compilation-filter-hook #'rustic-insert-errno-button nil t))

(defvar rustic-compilation-error
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for file paths containing '-->'.")

(defvar rustic-compilation-info
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *::: " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3 0)))) ;; 0 for info type
  "Create hyperlink in compilation buffers for file paths containing ':::'.")

(defvar rustic-compilation-panic
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^thread '[^']+' panicked at '[^']+', "
                      file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Match panics during compilation.")


;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation Process

(defvar rustic-compilation-process-name "rustic-compilation-process"
  "Process name for rust compilation processes.")

(defvar rustic-compilation-buffer-name "*rust-compilation*"
  "Buffer name for rust compilation process buffers.")

(defun rustic-compilation-start (command buffer process mode directory &optional sentinel)
  (let* ((buf (get-buffer-create buffer))
         (default-directory directory)
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list
                                (format "TERM=%s" "ansi")
                                (format "RUST_BACKTRACE=%s" rustic-compile-backtrace))
                               process-environment))
         (inhibit-read-only t))
    (setq next-error-last-buffer buf)
    (with-current-buffer buf
      (setq-local default-directory directory)
      (erase-buffer)
      (funcall mode)
      (funcall rustic-compile-display-method buf)
      (let ((proc (make-process :name process
                                :buffer buf
                                :command command
                                :filter #'rustic-compilation-filter
                                :sentinel (if sentinel sentinel #'compilation-sentinel))))
        (setq mode-line-process
              '((:propertize ":%s" face compilation-mode-line-run)
                compilation-mode-line-errors))
        (force-mode-line-update)
        proc))))

(defun rustic-compilation-filter (proc string)
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
    (when (eq major-mode 'rustic-mode)
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

(defun rustic-compilation-process-live (&optional nosave)
  "List live rustic processes."
  (let ((procs (list rustic-compilation-process-name
                     rustic-format-process-name
                     rustic-clippy-process-name
                     rustic-test-process-name))
        live)
    (setq live (-non-nil (cl-loop for proc in procs
                                  collect (let ((p (get-process proc)))
                                            (if (process-live-p p) p nil)))))
    (cl-assert (<= (length live) 1))
    (when live
      (rustic-process-kill-p (car live)))
    (unless nosave
      (rustic-save-some-buffers))
    live))

(defun rustic-process-kill-p (proc)
  "Don't allow two rust processes at once."
  (if (yes-or-no-p
       (format "`%s' is running; kill it? " proc))
      (condition-case ()
          (progn
            (interrupt-process proc)
            (sit-for 0.5)
            (delete-process proc))
        (error nil))
    (error "Cannot have two rust processes at once")))

(defun rustic-save-some-buffers ()
  "Unlike `save-some-buffers', only consider project related files. 

The variable `buffer-save-without-query' can be used for customization and
buffers are formatted after saving if `rustic-format-on-save' is t."
  (let* ((buffers (condition-case ()
                      (projectile-buffers-with-file (projectile-project-buffers))
                    (error nil))))
    (when-let (b (get-buffer rustic-format-buffer-name))
      (when (buffer-live-p b)
        (kill-buffer b)))
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer)
    	         (buffer-modified-p buffer))
        (with-current-buffer buffer
          (let ((saved-p nil))
            (let ((rustic-format-on-save nil))
              (setq saved-p
                    (if buffer-save-without-query
                        (progn (save-buffer) t)
                      (if (yes-or-no-p (format "Save file %s ? "
                                               (buffer-file-name buffer)))
                          (progn (save-buffer) t)
                        nil))))
            (when (and saved-p
                       rustic-format-on-save
                       (eq major-mode 'rustic-mode))
              (let* ((file (buffer-file-name buffer))
                     (proc (rustic-format-start-process buffer
                                                        'rustic-format-file-sentinel
                                                        nil
                                                        `(,rustic-rustfmt-bin ,file))))
                (while (eq (process-status proc) 'run)
                  (sit-for 0.1)))
              (revert-buffer t t))))))))

(defun rustic-save-some-buffers-advice (orig-fun &rest args)
  (let ((rustic-format-on-save nil))
    (apply orig-fun args)))

(advice-add 'save-some-buffers :around #'rustic-save-some-buffers-advice)

;;;;;;;;;;
;; Rustc

(defface rustic-errno-face
  '((t :foreground "red3"))
  "Error number face"
  :group 'rustic-compilation)

(defun rustic-insert-errno-button ()
  "Insert buttons in `rustic-compilation-mode'."
  (save-excursion
    (let ((start compilation-filter-start)
          (end (point)))
      (goto-char start)
      (save-match-data
        (while (re-search-forward (concat "error\\[E[0-9]+\\]") end t)
          (make-button (match-beginning 0)
                       (match-end 0)
                       :type 'rustc-errno))))))

(defun rustic-explain-error (button)
  "Open buffer with explanation for error at point."
  (let* ((button-string (button-label button))
         (errno (progn (string-match "E[0-9]+" button-string)
                       (match-string 0 button-string)))
         (buf (get-buffer-create "*rust errno*"))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (shell-command-to-string
               (concat "rustc --explain=" errno)))
      (markdown-view-mode)
      (setq
       header-line-format
       (concat (propertize " " 'display
                           `(space :align-to (- right-fringe ,(1+ (length errno)))))
               (propertize errno 'face 'rustic-errno-face)))
      (setq-local markdown-fontify-code-blocks-natively t)
      (setq-local markdown-fontify-code-block-default-mode 'rustic-mode)
      (markdown-toggle-markup-hiding 1)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(define-button-type 'rustc-errno
  'action #'rustic-explain-error
  'follow-link t
  'face 'rustic-errno-face
  'help-echo "mouse-1, RET: Explain errno")


;;;;;;;;;;;;;;;;
;; Interactive

;;;###autoload
(defun rustic-compile (&optional arg)
  "Compile rust project.
If called without arguments use `rustic-compile-command'.

Otherwise use provided argument ARG and store it in
`compilation-arguments'."
  (interactive "P")
  (let* ((command (setq compilation-arguments
                        (if arg
                            (read-from-minibuffer "Compile command: ")
                          rustic-compile-command)))
         (buffer-name rustic-compilation-buffer-name)
         (proc-name rustic-compilation-process-name)
         (mode 'rustic-compilation-mode)
         (dir (setq compilation-directory (rustic-buffer-workspace))))
    (rustic-compilation-process-live)
    (rustic-compilation-start
     (split-string command) buffer-name proc-name mode dir)))

;;;###autoload
(defun rustic-recompile ()
  "Re-compile the program using `compilation-arguments'."
  (interactive)
  (let* ((command (if (not compilation-arguments)
                      rustic-compile-command
                    compilation-arguments))
         (buffer-name rustic-compilation-buffer-name)
         (proc-name rustic-compilation-process-name)
         (mode 'rustic-compilation-mode)
         (dir (or compilation-directory default-directory)))
    (rustic-compilation-process-live)
    (rustic-compilation-start
     (split-string command) buffer-name proc-name mode dir)))

(provide 'rustic-compile)
;;; rustic-compile.el ends here
