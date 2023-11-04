;;; rustic-rustfmt.el --- Support for rustfmt         -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements support for `rustfmt', a tool that formats
;; Rust code according to style guidelines.

;;; Code:

(require 'project)

(require 'rustic-cargo)

;;; Options

(defcustom rustic-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rustic)

(defcustom rustic-rustfmt-bin-remote "~/.cargo/bin/rustfmt"
  "Path to remote rustfmt executable."
  :type 'string
  :group 'rustic)

(defun rustic-rustfmt-bin ()
  (if (file-remote-p (or (buffer-file-name) ""))
      rustic-rustfmt-bin-remote
    rustic-rustfmt-bin))

(defcustom rustic-rustfmt-args ""
  "String of additional arguments."
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
  "This option allows you to automatically run rustfmt when saving
or before using a compilation/cargo command.
`on-compile' calls 'cargo fmt' in the directory that is returned by
the function used in `rustic-compile-directory-method'."
  :type '(choice (const :tag "Format buffer before saving." on-save)
                 (const :tag "Run 'cargo fmt' before compilation." on-compile)
                 (const :tag "Don't format automatically." nil))
  :group 'rustic)

(defcustom rustic-format-on-save-method 'rustic-format-file
  "Default function used for formatting before saving.
This function will only be used when `rustic-format-trigger' is set
to 'on-save."
  :type 'function
  :group 'rustic)

(defcustom rustic-format-display-method 'pop-to-buffer
  "Default function used for displaying rustfmt buffer."
  :type 'function
  :group 'rustic)

(defcustom rustic-cargo-clippy-trigger-fix nil
  "Whether to run 'clippy --fix' before build or run."
  :type '(choice (const :tag "Run 'clippy --fix' before saving." on-save)
                 (const :tag "Run 'clippy --fix' before compilation." on-compile)
                 (const :tag "Don't fix automatically." nil))
  :group 'rustic)

(defcustom rustic-use-rust-save-some-buffers nil
  "Use `rustic-save-some-buffers' when calling `save-some-buffers' in rust
projects. It allows you to use automatic formatting for this function.
https://github.com/brotzeit/rustic/issues/450"
  :type 'boolean
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
  (rustic--inheritenv
   (let* ((err-buf (get-buffer-create rustic-format-buffer-name))
          (inhibit-read-only t)
          (dir (funcall rustic-compile-directory-method))
          (buffer (plist-get args :buffer))
          (string (plist-get args :stdin))
          (files  (plist-get args :files))
          (files (if (listp files) files (list files)))
          (command (or (plist-get args :command)
                       (rustic-compute-rustfmt-args)))
          (command (if (listp command) command (list command)))
          (cur-buf (current-buffer)))
     (setq rustic-save-pos (set-marker (make-marker) (point) (current-buffer)))
     (rustic-compilation-setup-buffer err-buf dir 'rustic-format-mode t)
     (--each files
       (unless (file-exists-p it)
         (error (format "File %s does not exist." it))))
     (with-current-buffer err-buf
       (let* ((c `(,(rustic-rustfmt-bin)
                   ,@(split-string rustic-rustfmt-args)
                   ,@command "--" ,@files))
              (proc (rustic-make-process :name rustic-format-process-name
                                         :buffer err-buf
                                         :command (remove "" c)
                                         :filter #'rustic-compilation-filter
                                         :sentinel sentinel
                                         :file-handler t)))
         (setq next-error-last-buffer buffer)
         (when string
           (process-put proc 'command-buf cur-buf)
           (while (not (process-live-p proc))
             (sleep-for 0.01))
           (process-send-string proc (concat string "\n"))
           (process-send-eof proc))
         proc)))))

(defun rustic-compute-rustfmt-args ()
  "Compute the arguments to rustfmt from `rustic-rustfmt-config-alist'."
  (let (args)
    (cl-dolist (elem rustic-rustfmt-config-alist args)
      (cl-destructuring-bind (key . val) elem
        (push (format "%s=%s" key (if (booleanp val) (if val "true" "false") val)) args)
        (push "--config" args)))))

(defun rustic-compute-rustfmt-file-lines-args (file start end)
  "Compute the arguments to rustfmt to modify a particular region."
  (list "--unstable-features"
    "--file-lines"
    (format "[{\"file\":\"%s\",\"range\":[%d,%d]}]" file start end)))

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
          (message "Rustfmt error."))))

    ;; rustfmt warnings
    (when-let ((b (process-get proc 'command-buf)))
      (when (process-get proc 'command-buf)
        (let ((warnings ""))
          (with-current-buffer b
            (save-excursion
              (goto-char (point-min))
              (while (looking-at "^Warning:")
                (setq warnings (concat warnings (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\n"))
                (kill-line)
                (delete-char 1)
                (goto-char (point-min)))))
          (message warnings))))))

(defun rustic-format-file-sentinel (proc output)
  "Sentinel for rustfmt processes when formatting a file."
  (ignore-errors
    (let ((proc-buffer (process-buffer proc)))
      (with-current-buffer proc-buffer
        (if (string-match-p "^finished" output)
            (and
             (with-current-buffer next-error-last-buffer
               (revert-buffer t t t))
             (kill-buffer proc-buffer))
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
  (let ((command (list (rustic-cargo-bin) "fmt"))
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
  (with-current-buffer (process-buffer proc)
    (setq default-directory (process-get proc 'workspace)))
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
                (revert-buffer t t t)))))
        (kill-buffer proc-buffer)
        (message "Workspace formatted with cargo-fmt.")))))

(defun rustic--get-line-number (pos)
  (let ((line-number 0))
    (save-excursion
      (goto-char pos)
      (setq line-number (string-to-number (format-mode-line "%l"))))
    line-number))

;;;###autoload
(defun rustic-format-region (begin end)
  "Format the current active region using rustfmt.

This operation requires a nightly version of rustfmt.
"
  (interactive "r")
  (unless (or (eq major-mode 'rustic-mode)
              (eq major-mode 'rustic-macro-expansion-mode))
    (error "Not a rustic-mode buffer."))
  (if (not (region-active-p)) (rustic-format-buffer)
    (let* ((buffer rustic-format-buffer-name)
           (file (buffer-file-name (current-buffer)))
           (mode 'rustic-format-mode)
           (proc rustic-format-process-name)
           (start (rustic--get-line-number begin))
           (finish (rustic--get-line-number end))
           (sentinel (lambda (proc output)
                       (let ((proc-buffer (process-buffer proc))
                             (inhibit-read-only t))
                         (with-current-buffer proc-buffer
                           (if (not (string-match-p "^finished" output))
                               (funcall rustic-compile-display-method proc-buffer)
                             (with-current-buffer (process-get proc 'file-buffer)
                               ;; turn off mark after region was formatted
                               ;; successfully
                               (setq mark-active nil)
                               (revert-buffer t t t))
                             (kill-buffer proc-buffer))))))
           (command (append (list (rustic-cargo-bin) "+nightly" "fmt" "--")
                            (rustic-compute-rustfmt-file-lines-args file
                                                                    start
                                                                    finish))))
      (rustic-compilation command (list :no-display t
                                        :buffer buffer
                                        :process proc
                                        :mode mode
                                        :sentinel sentinel)))))

;;;###autoload
(defun rustic-format-buffer ()
  "Format the current buffer using rustfmt."
  (interactive)
  (unless (or (eq major-mode 'rustic-mode)
              (eq major-mode 'rustic-macro-expansion-mode))
    (error "Not a rustic-mode buffer."))
  (rustic-compilation-process-live t)
  (save-excursion
    (rustic-format-start-process 'rustic-format-sentinel
                                 :buffer (current-buffer)
                                 :stdin (buffer-string))))

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

(defun rustic-format-dwim (beg end)
  "Format region if active, if not check if major mode is rustic
and format file, or else run 'cargo fmt'."
  (interactive "r")
  (cond ((region-active-p)
         (rustic-format-region beg end))
        ((eq major-mode 'rustic-mode)
         (rustic-format-file))
        (t
         (rustic-cargo-fmt))))

(defun rustic-project-root (project)
  "Runs the correct version of project-root function for
different emacs versions."
  (if (version<= emacs-version "28.0")
      (car (funcall 'project-roots project))
    (funcall 'project-root project)))

(defun rustic-project-buffer-list ()
  "Return a list of the buffers belonging to the current project.
This is basically a wrapper around `project--buffer-list'."
  (when-let ((pr (project-current)))
    (if (fboundp 'project--buffer-list)
        (project--buffer-list pr)
      ;; Like the above function but for releases before Emacs 28.
      (let ((root (rustic-project-root pr))
            bufs)
        (dolist (buf (buffer-list))
          (let ((filename (or (buffer-file-name buf)
                              (buffer-local-value 'default-directory buf))))
            (when (and filename (file-in-directory-p filename root))
              (push buf bufs))))
        (nreverse bufs)))))

;;; Hooks

(defun rustic-maybe-format-before-compilation (&optional clippy-fix)
  "Will be executed before running `rustic-compilation'."
  (let ((compile-ready-p t))
    ;; run clippy --fix, but only for "build" or "run" and rustic-compile
    (when (and clippy-fix
               (eq rustic-cargo-clippy-trigger-fix 'on-compile))
      (let* ((proc (rustic-cargo-clippy-fix :silent t :no-save t)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (unless (zerop (process-exit-status proc))
          (setq compile-ready-p nil))))

    ;; cargo fmt
    (when compile-ready-p
      (when (eq rustic-format-trigger 'on-compile)
        (let ((proc (rustic-cargo-fmt)))
          (while (eq (process-status proc) 'run)
            (sit-for 0.1))
          (unless (zerop (process-exit-status proc))
            (setq compile-ready-p nil)))))
    compile-ready-p))

(add-hook 'rustic-before-compilation-hook
          #'rustic-maybe-format-before-compilation)

(defun rustic-before-save-hook ()
  "Automatically run 'clippy --fix' OR rustfmt before saving.

Change `rustic-cargo-clippy-trigger-fix' and `rustic-format-trigger'
to make use of these features.

Don't throw error if rustfmt isn't installed, as it makes saving impossible."
  (when (eq rustic-cargo-clippy-trigger-fix 'on-save)
    (rustic-cargo-clippy-fix :silent t :no-save t))

  (unless (eq rustic-cargo-clippy-trigger-fix 'on-save)
    (save-excursion
      (when (and (rustic-format-on-save-p)
                 (not (rustic-compilation-process-live t)))
        (condition-case nil
            (progn
              (if (file-remote-p (buffer-file-name))
                  (rustic-format-buffer)
                (funcall rustic-format-on-save-method))
              (sit-for 0.1))
          (error nil))))))

(defun rustic-after-save-hook ()
  "Check if rustfmt is installed after saving the file."
  (when (rustic-format-on-save-p)
    (unless (executable-find (rustic-rustfmt-bin))
      (error "Could not locate executable \"%s\"" (rustic-rustfmt-bin)))))

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
  "Use `rustic-save-some-buffers' instead when called in rust project.
Otherwise turn off rustic format functionality and run `save-some-buffers'."
  ;; TODO: fix issue #450
  (if (and
       rustic-use-rust-save-some-buffers
       (rustic-buffer-crate t)
       (let ((pred (nth 1 args)))
         (if (functionp pred) (funcall pred) t)))
      (rustic-save-some-buffers)
    (let ((rustic-format-trigger nil)
          (rustic-format-on-save nil))
      (apply orig-fun args))))

(advice-add 'save-some-buffers :around
            #'rustic-save-some-buffers-advice)

;;; _
(provide 'rustic-rustfmt)
;;; rustic-rustfmt.el ends here
