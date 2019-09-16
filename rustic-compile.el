;;; rustic-compile.el --- Compile facilities -*-lexical-binding: t-*-

;;; Commentary:

;; Unlike compile.el, rustic makes use of a non dumb terminal in order to receive
;; all ANSI control sequences, which get translated by xterm-color.
;; This file also adds a derived compilation mode. Error matching regexes from
;; compile.el are removed.

;;; Code:

(require 'xterm-color)
(require 'projectile)
(require 'markdown-mode)

(require 'subr-x)

(require 'compile)


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

(defcustom rustic-compile-backtrace "0"
  "Set environment variable `RUST_BACKTRACE'."
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

(defcustom rustic-compilation-warning-face
  '((t :inherit default))
  "Override `compilation-warning-face' for rust compilation."
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
  (setq-local compilation-warning-face rustic-compilation-warning-face)
  (setq-local compilation-info-face rustic-compilation-info-face)
  (setq-local compilation-column-face rustic-compilation-line-face)
  (setq-local compilation-line-face rustic-compilation-column-face)
  
  (setq-local xterm-color-names-bright rustic-ansi-faces)
  (setq-local xterm-color-names rustic-ansi-faces)

  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-error rustic-compilation-error))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-warning rustic-compilation-warning))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-info rustic-compilation-info))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustic-panic rustic-compilation-panic))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'rustic-error)
  (add-to-list 'compilation-error-regexp-alist 'rustic-warning)
  (add-to-list 'compilation-error-regexp-alist 'rustic-info)
  (add-to-list 'compilation-error-regexp-alist 'rustic-panic)

  (add-hook 'compilation-filter-hook #'rustic-insert-errno-button nil t))

(defvar rustic-compilation-error
  (let ((err "^error[^:]*:[^\n]*\n\s*-->\s")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat err file ":" start-line ":" start-col)))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for rust errors.")

(defvar rustic-compilation-warning
  (let ((warning "^warning:[^\n]*\n\s*-->\s")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat warning file ":" start-line ":" start-col)))
      (cons re '(1 2 3 1)))) ;; 1 for warning
  "Create hyperlink in compilation buffers for rust warnings.")

(defvar rustic-compilation-info
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *::: " file ":" start-line ":" start-col)))
      (cons re '(1 2 3 0)))) ;; 0 for info type
  "Create hyperlink in compilation buffers for file paths preceded by ':::'.")

(defvar rustic-compilation-panic
  (let ((panic "thread '[^']+' panicked at '[^']+', ")
        (file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat panic file ":" start-line ":" start-col)))
      (cons re '(1 2 3))))
  "Match thread panics.")


;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation Process

(defvar rustic-compilation-process-name "rustic-compilation-process"
  "Process name for rust compilation processes.")

(defvar rustic-compilation-buffer-name "*rustic-compilation*"
  "Buffer name for rust compilation process buffers.")

(defun rustic-make-process (&rest args)
  "Wrapper for `make-process'.

Set environment variables for rust process."
  (let ((coding-system-for-read 'binary)
        (process-environment (nconc
	                          (list
                               (format "TERM=%s" "ansi")
                               (format "RUST_BACKTRACE=%s" rustic-compile-backtrace))
                              process-environment)))
    (make-process :name (plist-get args :name)
                  :buffer (plist-get args :buffer)
                  :command (plist-get args :command)
                  :filter (plist-get args :filter)
                  :sentinel (plist-get args :sentinel)
                  :coding 'utf-8-emacs-unix)))

(defun rustic-compilation-setup-buffer (buf dir mode &optional no-mode-line)
  "Prepare BUF for compilation process."
  (let ((inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory dir)
      (funcall mode)
      (unless no-mode-line
        (setq mode-line-process
              '((:propertize ":%s" face compilation-mode-line-run)
                compilation-mode-line-errors)))
      (force-mode-line-update)
      (sit-for 0))))

(defun rustic-compilation-start (command &rest args)
  "Start a compilation process with COMMAND."
  (let ((buf (get-buffer-create
              (or (plist-get args :buffer) rustic-compilation-buffer-name)))
        (process (or (plist-get args :process) rustic-compilation-process-name))
        (mode (or (plist-get args :mode) 'rustic-compilation-mode))
        (directory (or (plist-get args :directory) (rustic-buffer-workspace)))
        (sentinel (or (plist-get args :sentinel) #'compilation-sentinel)))
    (when compilation-scroll-output
      (rustic-compilation-setup-buffer buf directory mode))
    (funcall rustic-compile-display-method buf)
    (unless compilation-scroll-output
      (rustic-compilation-setup-buffer buf directory mode))
    (with-current-buffer buf
      (rustic-make-process :name process
                           :buffer buf
                           :command command
                           :filter #'rustic-compilation-filter
                           :sentinel sentinel))))

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
              (compilation--ensure-parse (point-max))

              (unless comint-inhibit-carriage-motion
                (comint-carriage-motion (process-mark proc) (point)))
              (set-marker (process-mark proc) (point))
              (run-hooks 'compilation-filter-hook))
	      (goto-char pos)
          (narrow-to-region min max)
	      (set-marker pos nil)
	      (set-marker min nil)
	      (set-marker max nil))))))

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

(defun rustic-process-kill-p (proc &optional no-error)
  "Don't allow two rust processes at once.

If NO-ERROR is t, don't throw error if user chooses not to kill running process."
  (if (yes-or-no-p
       (format "`%s' is running; kill it? " proc))
      (condition-case ()
          (progn
            (interrupt-process proc)
            (sit-for 0.5)
            (delete-process proc))
        (error nil))
    (unless no-error
      (error "Cannot have two rust processes at once"))))

(defun rustic-save-some-buffers ()
  "Unlike `save-some-buffers', only consider project related files.

The variable `buffer-save-without-query' can be used for customization and
buffers are formatted after saving if `rustic-format-on-save' is t."
  (let ((buffers (condition-case ()
                     (projectile-buffers-with-file (projectile-project-buffers))
                   (buffer-list)))
        (b (get-buffer rustic-format-buffer-name)))
    (when (buffer-live-p b)
      (kill-buffer b))
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
                     (proc (rustic-format-start-process 'rustic-format-file-sentinel
                                                        :buffer buffer
                                                        :command `(,rustic-rustfmt-bin ,file))))
                (while (eq (process-status proc) 'run)
                  (sit-for 0.1))))))))))

;; disable formatting for save-some-buffers
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
         (dir (setq compilation-directory (rustic-buffer-workspace))))
    (rustic-compilation-process-live)
    (rustic-compilation-start (split-string command) :directory dir)))

;;;###autoload
(defun rustic-recompile ()
  "Re-compile the program using `compilation-arguments'."
  (interactive)
  (let* ((command (or compilation-arguments rustic-compile-command))
         (dir compilation-directory))
    (rustic-compilation-process-live)
    (rustic-compilation-start (split-string command) :directory dir)))

(provide 'rustic-compile)
;;; rustic-compile.el ends here
