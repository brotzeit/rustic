;;; rustic-rustfmt.el --- Support for rustfmt         -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements support for `rustfmt', a tool that formats
;; Rust code according to style guidelines.

;;; Code:

(require 'rustic-cargo)

(declare-function project-root "project")

;;; Options

(defcustom rustic-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rustic)

(defcustom rustic-rustfmt-config-alist nil
  "An alist of (KEY . VAL) pairs that are passed to rustfmt.

KEY is a symbol that corresponds to a config value of rustfmt.
VALUE is a string, an integer or a boolean."
  :type '(alist :key-type symbol
                :value-type (choice string integer boolean))
  :group 'rustic)

(defcustom rustic-format-trigger nil
  "Format future rust buffers before saving using rustfmt."
  :type '(choice (const :tag "Format buffer before saving." on-save)
                 (const :tag "Run 'cargo fmt' before compilation." on-compile)
                 (const :tag "Don't format automatically." nil))
  :group 'rustic)

(defcustom rustic-format-display-method 'pop-to-buffer
  "Default function used for displaying rustfmt buffer."
  :type 'function
  :group 'rustic)

;;; _

(defvar rustic-format-process-name "rustic-rustfmt-process"
  "Process name for rustfmt processes.")

(defvar rustic-format-buffer-name "*rustfmt*"
  "Buffer name for rustfmt process buffers.")

(defvar rustic-save-pos nil
  "Marker, holding location of the cursor's position before
running rustfmt.")

(defun rustic-format-start-process (sentinel &rest args)
  "Run rustfmt with ARGS.

:buffer BUFFER -- BUFFER is the buffer that is being formatted.

:stdin STRING -- STRING will be written to the standard input of rustfmt.
When `:files' is non-nil, STRING will be ignored by rustfmt.

:files FILES -- FILES is a string or list of strings that
specify the input file or files to rustfmt.

:command COMMAND -- COMMAND is a string or a list of strings.
When COMMAND is non-nil, it replaces the default command.
When COMMAND is a string, it is the program file name.
When COMMAND is a list, it's `car' is the program file name
and it's `cdr' is a list of arguments."
  (let* ((err-buf (get-buffer-create rustic-format-buffer-name))
         (inhibit-read-only t)
         (dir (rustic-buffer-workspace))
         (buffer (plist-get args :buffer))
         (string (plist-get args :stdin))
         (files  (plist-get args :files))
         (files (if (listp files) files (list files)))
         (command (or (plist-get args :command)
                      (cons rustic-rustfmt-bin (rustic-compute-rustfmt-args))))
         (command (if (listp command) command (list command))))
    (setq rustic-save-pos (set-marker (make-marker) (point) (current-buffer)))
    (rustic-compilation-setup-buffer err-buf dir 'rustic-format-mode t)
    (--each files
      (unless (file-exists-p it)
        (error (format "File %s does not exist." it))))
    (with-current-buffer err-buf
      (let ((proc (rustic-make-process :name rustic-format-process-name
                                       :buffer err-buf
                                       :command `(,@command "--" ,@files)
                                       :filter #'rustic-compilation-filter
                                       :sentinel sentinel)))
        (setq next-error-last-buffer buffer)
        (when string
          (while (not (process-live-p proc))
            (sleep-for 0.01))
          (process-send-string proc (concat string "\n"))
          (process-send-eof proc))
        proc))))

(defun rustic-compute-rustfmt-args ()
  "Compute the arguments to rustfmt from `rustic-rustfmt-config-alist'."
  (let (args)
    (cl-dolist (elem rustic-rustfmt-config-alist args)
      (cl-destructuring-bind (key . val) elem
        (push (format "%s=%s" key (if (booleanp val) (if val "true" "false") val)) args)
        (push "--config" args)))))

(defun rustic-format-sentinel (proc output)
  "Sentinel for rustfmt processes when using stdin."
  (ignore-errors
    (let ((proc-buffer (process-buffer proc))
          (inhibit-read-only t))
      (with-current-buffer proc-buffer
        (if (string-match-p "^finished" output)
            (let ((file-buffer next-error-last-buffer)
                  ;; replace-buffer-contents was in emacs 26.1, but it
                  ;; was broken for non-ASCII strings, so we need 26.2.
                  (use-replace (version<= "26.2" emacs-version)))
              (unless use-replace
                (copy-to-buffer file-buffer (point-min) (point-max)))
              (with-current-buffer file-buffer
                (if use-replace
                    (replace-buffer-contents proc-buffer))
                (goto-char rustic-save-pos))
              (kill-buffer proc-buffer)
              (message "Formatted buffer with rustfmt."))
          (goto-char (point-min))
          (when-let ((file (buffer-file-name next-error-last-buffer)))
            (save-excursion
              (save-match-data
                (when (search-forward "<stdin>" nil t)
                  (replace-match file)))))
          (with-current-buffer next-error-last-buffer
            (goto-char rustic-save-pos))
          (funcall rustic-format-display-method proc-buffer)
          (message "Rustfmt error."))))))

(defun rustic-format-file-sentinel (proc output)
  "Sentinel for rustfmt processes when formatting a file."
  (ignore-errors
    (let ((proc-buffer (process-buffer proc)))
      (with-current-buffer proc-buffer
        (if (string-match-p "^finished" output)
            (with-current-buffer next-error-last-buffer
              (revert-buffer t t))
          (sit-for 0.1)
          (with-current-buffer next-error-last-buffer
            (goto-char rustic-save-pos))
          (goto-char (point-min))
          (funcall rustic-format-display-method proc-buffer)
          (message "Rustfmt error."))))))

(define-derived-mode rustic-format-mode rustic-compilation-mode "rustfmt"
  :group 'rustic)

(define-derived-mode rustic-cargo-fmt-mode rustic-compilation-mode "cargo-fmt"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-fmt ()
  "Use rustfmt via cargo."
  (interactive)
  (let ((command (list rustic-cargo-bin "fmt"))
        (buffer rustic-format-buffer-name)
        (proc rustic-format-process-name)
        (mode 'rustic-cargo-fmt-mode))
    (rustic-compilation-process-live)
    (rustic-compilation command
                        (list
                         :no-display t
                         :buffer buffer
                         :process proc
                         :mode mode
                         :sentinel #'rustic-cargo-fmt-sentinel))))

(defun rustic-cargo-fmt-sentinel (proc output)
  "Sentinel for formatting with `rustic-cargo-fmt'."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer proc-buffer
      (if (not (string-match-p "^finished" output))
          (funcall rustic-compile-display-method proc-buffer)
        (when (fboundp rustic-list-project-buffers-function)
          (let ((buffers (cl-remove-if-not
                          #'buffer-file-name
                          (funcall rustic-list-project-buffers-function))))
            (dolist (b buffers)
              (with-current-buffer b
                (revert-buffer t t)))))
        (kill-buffer proc-buffer)
        (message "Workspace formatted with cargo-fmt.")))))

;;;###autoload
(defun rustic-format-buffer ()
  "Format the current buffer using rustfmt.

Provide optional argument NO-STDIN for `rustic-before-save-hook' since there
were issues when using stdin for formatting."
  (interactive)
  (unless (or (eq major-mode 'rustic-mode)
              (eq major-mode 'rustic-macro-expansion-mode))
    (error "Not a rustic-mode buffer."))
  (rustic-compilation-process-live t)
  (rustic-format-start-process 'rustic-format-sentinel
                               :buffer (current-buffer)
                               :stdin (buffer-string)))

;;;###autoload
(defun rustic-format-file (&optional file)
  "Unlike `rustic-format-buffer' format file directly and revert the buffer."
  (interactive "P")
  (let* ((buf (current-buffer))
         (file (or (if file (read-from-minibuffer "Format file: ") nil)
                   (buffer-file-name buf)
                   (read-from-minibuffer "Format file: ")))
         (string (buffer-string)))
    (write-region string nil file nil 0)
    (let ((proc (rustic-format-start-process 'rustic-format-file-sentinel
                                             :buffer buf
                                             :files file)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.05)))))

(defun rustic-project-buffer-list ()
  "Return a list of the buffers belonging to the current project.
This is basically a wrapper around `project--buffer-list'."
  (when-let ((pr (project-current)))
    (if (fboundp 'project--buffer-list)
        (project--buffer-list pr)
      ;; Like the above function but for releases before Emacs 28.
      (let ((root (project-root pr))
            bufs)
        (dolist (buf (buffer-list))
          (let ((filename (or (buffer-file-name buf)
                              (buffer-local-value 'default-directory buf))))
            (when (and filename (file-in-directory-p filename root))
              (push buf bufs))))
        (nreverse bufs)))))

;;; Hooks

(defun rustic-maybe-format-before-compilation ()
  (if (eq rustic-format-trigger 'on-compile)
      (let ((proc (rustic-cargo-fmt)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (and (not (zerop (process-exit-status proc)))
             (funcall rustic-compile-display-method (process-buffer proc))
             t))
    t))

(add-hook 'rustic-before-compilation-hook
          #'rustic-maybe-format-before-compilation)

(defun rustic-before-save-hook ()
  "Don't throw error if rustfmt isn't installed, as it makes saving impossible."
  (when (and (rustic-format-on-save-p)
             (not (rustic-compilation-process-live t)))
    (condition-case nil
        (progn
          (rustic-format-file)
          (sit-for 0.1))
      (error nil))))

(defun rustic-after-save-hook ()
  "Check if rustfmt is installed after saving the file."
  (when (rustic-format-on-save-p)
    (unless (executable-find rustic-rustfmt-bin)
      (error "Could not locate executable \"%s\"" rustic-rustfmt-bin))))

(defun rustic-maybe-format-after-save (buffer)
  (when (rustic-format-on-save-p)
    (let* ((file (buffer-file-name buffer))
           (proc (rustic-format-start-process
                  'rustic-format-file-sentinel
                  :buffer buffer
                  :files file)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))))

(defvar rustic-format-on-save nil
  "Format rust buffers before saving using rustfmt.")

(make-obsolete 'rustic-format-on-save 'rustic-format-trigger "Rustic 0.19")

(defun rustic-format-on-save-p ()
  "Return non-nil if formatting should happen when saving.
See option `rustic-format-trigger'.  For backward compatibility,
if obsolete `rustic-format-on-save' is non-nil, then also return
non-nil."
  (or rustic-format-on-save (eq rustic-format-trigger 'on-save)))

(defun rustic-save-some-buffers-advice (orig-fun &rest args)
  (let ((rustic-format-trigger nil)
        (rustic-format-on-save nil))
    (apply orig-fun args)))

(advice-add 'save-some-buffers :around
            #'rustic-save-some-buffers-advice)

;;; _
(provide 'rustic-rustfmt)
;;; rustic-rustfmt.el ends here
