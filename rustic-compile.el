;;; rustic-compile.el --- Compile facilities -*-lexical-binding: t-*-

;;; Commentary:

;; Unlike compile.el, rustic makes use of a non dumb terminal in order to receive
;; all ANSI control sequences, which get translated by xterm-color.
;; This file also adds a derived compilation mode. Error matching regexes from
;; compile.el are removed.

;;; Code:

(require 'markdown-mode)
(require 'xterm-color)
(require 's)

(require 'compile)

(require 'rustic)

(defvar rustic-format-trigger)
(defvar rustic-format-on-save)

;;; Customization

(defgroup rustic-compilation nil
  "Rust Compilation."
  :group 'rustic
  :group 'processes)

(defcustom rustic-compile-command (purecopy "cargo build")
  "Default command for rust compilation."
  :type 'string
  :group 'rustic-compilation)

(defcustom rustic-compile-command-remote "~/.cargo/bin/cargo"
  "Default command for remote rust compilation."
  :type 'string
  :group 'rustic-compilation)

(defun rustic-compile-command ()
  (if (file-remote-p (or (buffer-file-name) ""))
      rustic-compile-command-remote
    rustic-compile-command))

(defcustom rustic-compile-display-method 'display-buffer
  "Default function used for displaying compilation buffer."
  :type 'function
  :group 'rustic-compilation)

(defcustom rustic-compile-backtrace (or (getenv "RUST_BACKTRACE") "0")
  "Set environment variable `RUST_BACKTRACE'."
  :type '(choice (string :tag "0")
                 (string :tag "1")
                 (string :tag "full"))
  :group 'rustic-compilation)

(defcustom rustic-compile-rustflags (or (getenv "RUSTFLAGS") "")
  "String used for RUSTFLAGS."
  :type 'string
  :group 'rustic-compilation)

(defcustom rustic-list-project-buffers-function
  (if (fboundp 'projectile-project-buffers)
      'projectile-project-buffers
    'rustic-project-buffer-list)
  "Function used to list buffers belonging to current project."
  :type '(choice (const projectile-project-buffers)
                 (const rustic-project-buffer-list)
                 function)
  :group 'rustic)


;;; Faces

(define-obsolete-face-alias 'rustic-message-face
  'rustic-message "1.2")
(define-obsolete-face-alias 'rustic-compilation-error-face
  'rustic-compilation-error "1.2")
(define-obsolete-face-alias 'rustic-compilation-warning-face
  'rustic-compilation-warning "1.2")
(define-obsolete-face-alias 'rustic-compilation-info-face
  'rustic-compilation-info "1.2")
(define-obsolete-face-alias 'rustic-compilation-line-face
  'rustic-compilation-line "1.2")
(define-obsolete-face-alias 'rustic-compilation-column-face
  'rustic-compilation-column "1.2")

(defface rustic-message
  '((t :inherit default))
  "Don't use `compilation-message-face', as ansi colors get messed up."
  :group 'rustic-compilation)

(defface rustic-compilation-error
  '((t :inherit default))
  "Override `compilation-error-face' for rust compilation."
  :group 'rustic-compilation)

(defface rustic-compilation-warning
  '((t :inherit default))
  "Override `compilation-warning-face' for rust compilation."
  :group 'rustic-compilation)

(defface rustic-compilation-info
  '((t :inherit default))
  "Override `compilation-info-face' for rust compilation."
  :group 'rustic-compilation)

(defface rustic-compilation-line
  '((t :inherit default))
  "Override `compilation-line-face' for rust compilation."
  :group 'rustic-compilation)

(defface rustic-compilation-column
  '((t :inherit default))
  "Override `compilation-column-face' for rust compilation."
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

;;; Compilation-mode

(defvar rustic-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map compilation-mode-map)
    (define-key map "p" 'rustic-popup)
    (define-key map "g" 'rustic-recompile)
    map)
  "Keymap for rust compilation log buffers.")

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

(define-compilation-mode rustic-compilation-mode "rust-compilation"
  "Rust compilation mode.

Error matching regexes from compile.el are removed."
  (setq-local compilation-message-face 'rustic-message)
  (setq-local compilation-error-face   'rustic-compilation-error)
  (setq-local compilation-warning-face 'rustic-compilation-warning)
  (setq-local compilation-info-face    'rustic-compilation-info)
  (setq-local compilation-column-face  'rustic-compilation-column)
  (setq-local compilation-line-face    'rustic-compilation-line)

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

  (add-hook 'compilation-filter-hook #'rustic-insert-errno-button nil t)

  (setq-local rustic-compilation-workspace (rustic-buffer-workspace)))

;;; Compilation Process

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

    (when (> (length rustic-compile-rustflags) 0)
      (setq process-environment
            (nconc (list (format "RUSTFLAGS=%s" rustic-compile-rustflags))
                   process-environment)))

    (let ((process (apply
                    #'start-file-process (plist-get args :name)
                    (plist-get args :buffer)
                    (plist-get args :command))))
      (set-process-filter process (plist-get args :filter))
      (set-process-sentinel process (plist-get args :sentinel))
      (set-process-coding-system process 'utf-8-emacs-unix 'utf-8-emacs-unix)
      (process-put process 'command (plist-get args :command))
      (process-put process 'workspace (plist-get args :workspace))
      (process-put process 'file-buffer (plist-get args :file-buffer))
      process)))

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
      (if (or compilation-auto-jump-to-first-error
              (eq compilation-scroll-output 'first-error))
          (set (make-local-variable 'compilation-auto-jump-to-next) t))
      (sit-for 0))))

(defvar rustic-before-compilation-hook nil)

(defun rustic-compilation-start (command &optional args)
  "Start a compilation process COMMAND with ARGS.
ARGS is a plist that affects how the process is run,
see `rustic-compilation' for details.  First run
`rustic-before-compilation-hook' and if any of these
functions fails, then do not start compilation."
  (save-excursion
    (when (run-hook-with-args-until-failure 'rustic-before-compilation-hook (plist-get args :clippy-fix))
      (rustic-compilation command args))))

(defun rustic-compilation (command &optional args)
  "Start a compilation process with COMMAND.

ARGS is a plist that affects how the process is run.
- `:no-display' don't display buffer when starting compilation process
- `:buffer' name for process buffer
- `:process' name for compilation process
- `:mode' mode for process buffer
- `:directory' set `default-directory'
- `:sentinel' process sentinel"
  (let* ((buf (get-buffer-create
               (or (plist-get args :buffer) rustic-compilation-buffer-name)))
         (process (or (plist-get args :process) rustic-compilation-process-name))
         (mode (or (plist-get args :mode) 'rustic-compilation-mode))
         (directory (or (plist-get args :directory) (funcall rustic-compile-directory-method)))
         (workspace (rustic-buffer-workspace))
         (sentinel (or (plist-get args :sentinel) #'rustic-compilation-sentinel))
         (file-buffer (current-buffer)))
    (rustic-compilation-setup-buffer buf directory mode)
    (setq next-error-last-buffer buf)
    (unless (plist-get args :no-display)
      (funcall rustic-compile-display-method buf))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert (format "%s \n" (s-join " "  command))))
      (rustic-make-process :name process
                           :buffer buf
                           :command command
                           :file-buffer file-buffer
                           :filter #'rustic-compilation-filter
                           :sentinel sentinel
                           :workspace workspace
                           :file-handler t))))

(defun rustic-compilation-filter (proc string)
  "Insert the text emitted by PROC.
Translate STRING with `xterm-color-filter'."
  (let ((buffer (process-buffer proc))
        buffer-empty-p)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (= (buffer-size) 0)
          (setq buffer-empty-p t))
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
            (set-marker max nil))))
      ;; Issue #101: set window point to `point-min' when `compilation-scroll-output' is nil
      (when (and (not compilation-scroll-output) buffer-empty-p)
        (let ((win (get-buffer-window buffer)))
          (set-window-start win (point-min))
          (set-window-point win (point-min)))))))

;; we only use this sentinel to set `default-directory' to the workspace
;; dir to ensure goto-error functions work
(defun rustic-compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (let ((buffer (process-buffer proc)))
    (with-current-buffer buffer
      (setq default-directory (process-get proc 'workspace)))
    (if (memq (process-status proc) '(exit signal))
	    (if (null (buffer-name buffer))
	        ;; buffer killed
	        (set-process-buffer proc nil)
	      (with-current-buffer buffer
	        ;; Write something in the compilation buffer
	        ;; and hack its mode line.
	        (compilation-handle-exit (process-status proc)
				                     (process-exit-status proc)
				                     msg)
	        ;; Since the buffer and mode line will show that the
	        ;; process is dead, we can delete it now.  Otherwise it
	        ;; will stay around until M-x list-processes.
	        (delete-process proc)))
      (setq compilation-in-progress (delq proc compilation-in-progress))
      (compilation--update-in-progress-mode-line))))

(defun rustic-compilation-process-live (&optional nosave)
  "Ask to kill live rustic process if any and call `rustic-save-some-buffers'.
If optional NOSAVE is non-nil, then do not do the latter.
Return non-nil if there was a live process."
  (let ((procs (mapcan (lambda (proc)
                         (and proc
                              (let ((proc (get-process proc)))
                                (and (process-live-p proc)
                                     (list proc)))))
                       (list rustic-compilation-process-name
                             (bound-and-true-p rustic-format-process-name)
                             (bound-and-true-p rustic-clippy-process-name)
                             (bound-and-true-p rustic-run-process-name)
                             (bound-and-true-p rustic-test-process-name)
                             (bound-and-true-p rustic-expand-process-name)))))
    (when (> (length procs) 1)
      (error "BUG: Multiple live rustic processes: %s" procs))
    (when procs
      (rustic-process-kill-p (car procs)))
    (unless nosave
      (rustic-save-some-buffers))
    procs))

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

The variable `compilation-ask-about-save' can be used for customization and
buffers are formatted after saving if turned on by `rustic-format-trigger'."
  (let ((buffers (cl-remove-if-not
                  #'buffer-file-name
                  (if (fboundp rustic-list-project-buffers-function)
                      (funcall rustic-list-project-buffers-function)
                    (buffer-list))))
        (b (get-buffer (bound-and-true-p rustic-format-buffer-name))))
    (when (buffer-live-p b)
      (kill-buffer b))
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer)
                 (buffer-modified-p buffer))
        (with-current-buffer buffer
          (let ((saved-p nil))
            ;; also set rustic-format-on-save for backwards compatibility
            (let ((rustic-format-trigger nil)
                  (rustic-format-on-save nil))
              (setq saved-p
                    (if (not compilation-ask-about-save)
                        (progn (save-buffer) t)
                      (if (yes-or-no-p (format "Save file %s ? "
                                               (buffer-file-name buffer)))
                          (progn (save-buffer) t)
                        nil))))
            (when (and saved-p
                       (eq major-mode 'rustic-mode)
                       (fboundp 'rustic-maybe-format-after-save))
              (rustic-maybe-format-after-save buffer))))))))

(defun rustic-compile-goto-error-hook (orig-fun &rest args)
  "Provide possibility use `compile-goto-error' on line numbers in compilation buffers.
This hook checks if there's a line number at the beginning of the
current line in an error section."
  (-if-let* ((rustic-p (eq major-mode 'rustic-compilation-mode))
             (default-directory rustic-compilation-workspace)
             (line-contents (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))
             (line-number-p (string-match "^[0-9]+\s+\|" line-contents))
             (line-number (car (split-string line-contents))))
      (save-excursion
        ;; find compilation message in error
        (while (not (or (get-text-property (point) 'compilation-message)
                        (bobp)))
          (forward-line -1))
        ;; get file of text property
        (let* ((msg (get-text-property (point) 'compilation-message))
               (loc (compilation--message->loc msg))
               (file (caar (compilation--loc->file-struct loc))))
          ;; open file of error and goto line number that we parsed from the line we are on
          (with-current-buffer (find-file-other-window file)
            (save-restriction
              (widen)
              (goto-char (point-min))
              (forward-line (1- (string-to-number line-number)))))))
    (apply orig-fun args)))

(advice-add 'compile-goto-error :around #'rustic-compile-goto-error-hook)

(defun rustic-compile-send-input ()
  "Read string from minibuffer and send it to the rust process of the current
buffer."
  (interactive)
  (let ((input (read-from-minibuffer "Send input to rust process: "))
        (proc (get-buffer-process (current-buffer)))
        (inhibit-read-only t))
    (process-send-string proc (concat input "\n"))))

;;; Rustc

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

;;; Interactive

;;;###autoload
(defun rustic-compile (&optional arg)
  "Compile rust project.

If `compilation-read-command' is non-nil or if called with prefix
argument ARG then read the command in the minibuffer.  Otherwise
use `rustic-compile-command'.

In either store the used command in `compilation-arguments'."
  (interactive "P")
  (rustic-set-compilation-arguments
        (if (or compilation-read-command arg)
            (compilation-read-command (or (car compilation-arguments)
                                          (rustic-compile-command)))
          (rustic-compile-command)))
  (setq compilation-directory (funcall rustic-compile-directory-method))
  (rustic-compilation-process-live)
  (rustic-compilation-start (split-string (car compilation-arguments))
                            (list :directory compilation-directory
                                  :clippy-fix t)))

;; TODO: we don't use the other options stored in `compilation-arguments',
;;       but probably we should
(defun rustic-set-compilation-arguments (command)
  "Set `compilation-arguments' using COMMAND.
It's a list that looks like (list command mode name-function highlight-regexp)."
  ;; TODO: compile.el uses setq-local, but that does seem weird right ?
  (setq compilation-arguments (list command nil nil nil)))

;;;###autoload
(defun rustic-recompile ()
  "Re-compile the program using `compilation-arguments'."
  (interactive)
  (let* ((command (or (car compilation-arguments) (rustic-compile-command)))
         (dir compilation-directory))
    (rustic-compilation-process-live)
    (rustic-compilation-start (split-string command)
                              (list :directory dir :clippy-fix t))))

;;; Spinner

(require 'spinner)

(defcustom rustic-display-spinner t
  "Display spinner."
  :type 'boolean
  :group 'rustic)

(defcustom rustic-spinner-type 'horizontal-moving
  "Holds the type of spinner to be used in the mode-line.
Takes a value accepted by `spinner-start'."
  :type `(choice (choice :tag "Choose a spinner by name"
                         ,@(mapcar (lambda (c) (list 'const (car c)))
                                   spinner-types))
                 (const :tag "A random spinner" random)
                 (repeat :tag "A list of symbols from `spinner-types' to randomly choose from"
                         (choice :tag "Choose a spinner by name"
                                 ,@(mapcar (lambda (c) (list 'const (car c)))
                                           spinner-types)))
                 (vector :tag "A user defined vector"
                         (repeat :inline t string))))

(defmacro rustic-with-spinner (spinner val mode-line &rest body)
  (declare (indent defun))
  `(when rustic-display-spinner
     (when (spinner-p ,spinner)
       (spinner-stop ,spinner))
     (setq ,spinner ,val)
     (setq mode-line-process ,mode-line)
     ,@body))

;;; _
(provide 'rustic-compile)
;;; rustic-compile.el ends here
