;;; rustic-cargo.el --- Cargo based commands -*-lexical-binding: t-*-
;;; Commentary:

;; This library implements support for `cargo'.

;;; Code:

(require 'tabulated-list)
(require 'dash)
(require 's)

(require 'rustic-compile)
(require 'rustic-interaction) ; for rustic-beginning-of-function

;;; Customization

(defcustom rustic-cargo-bin "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-check-exec-command "check"
  "Execute command to run cargo check."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-test-exec-command "test"
  "Execute command to run cargo test."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-run-exec-command "run"
  "Execute command to run cargo run."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-build-exec-command "build"
  "Execute command to run cargo build."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-bin-remote "~/.cargo/bin/cargo"
  "Path to remote cargo executable."
  :type 'string
  :group 'rustic-cargo)

(defun rustic-cargo-bin ()
  (if (file-remote-p (or (buffer-file-name) ""))
      rustic-cargo-bin-remote
    rustic-cargo-bin))

(defcustom rustic-cargo-open-new-project t
  "If t then any project created with cargo-new will be opened automatically.
If nil then the project is simply created."
  :type 'boolean
  :group 'rustic-cargo)

(defcustom rustic-cargo-test-disable-warnings nil
  "Don't show warnings when running 'cargo test'."
  :type 'boolean
  :group 'rustic-cargo)

(defcustom rustic-default-test-arguments "--benches --tests --all-features"
  "Default arguments when running 'cargo test'."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-default-install-arguments '("--path" ".")
  "Default arguments when running 'cargo install'."
  :type '(list string)
  :group 'rustic-cargo)

(defcustom rustic-cargo-check-arguments "--benches --tests --all-features"
  "Default arguments when running 'cargo check'."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-build-arguments ""
  "Default arguments when running 'cargo build'."
  :type 'string
  :group 'rustic-cargo)

(defvar rustic-cargo-outdated-face nil)
(make-obsolete-variable 'rustic-cargo-outdated-face
                        "use the face `rustic-cargo-outdated' instead."
                        "1.2")

(defface rustic-cargo-outdated
  '((t (:foreground "red")))
  "Face used for outdated crates."
  :group 'rustic)

(define-obsolete-face-alias 'rustic-cargo-outdated-upgrade-face
  'rustic-cargo-outdated-upgrade "1.2")

(defface rustic-cargo-outdated-upgrade
  '((t (:foreground "LightSeaGreen")))
  "Face used for crates marked for upgrade."
  :group 'rustic)

;;; Test

(defvar rustic-test-process-name "rustic-cargo-test-process"
  "Process name for test processes.")

(defvar rustic-test-buffer-name "*cargo-test*"
  "Buffer name for test buffers.")

(defvar rustic-test-arguments ""
  "Holds arguments for 'cargo test', similar to `compilation-arguments`.
Tests that are executed by `rustic-cargo-current-test' will also be
stored in this variable.")

(defvar rustic-cargo-test-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map rustic-compilation-mode-map)
    (define-key map [remap recompile] 'rustic-cargo-test-rerun)
    map)
  "Local keymap for `rustic-cargo-test-mode' buffers.")

(define-derived-mode rustic-cargo-test-mode rustic-compilation-mode "cargo-test"
  :group 'rustic

  ;; TODO: append to rustic-compile-rustflags
  (when rustic-cargo-test-disable-warnings
    (setq-local rustic-compile-rustflags "-Awarnings")))

(defun rustic-cargo-run-test (test)
  "Run TEST which can be a single test or mod name."
  (let* ((c (list (rustic-cargo-bin) rustic-cargo-test-exec-command test))
         (buf rustic-test-buffer-name)
         (proc rustic-test-process-name)
         (mode 'rustic-cargo-test-mode))
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-test-run (&optional test-args)
  "Start compilation process for 'cargo test' with optional TEST-ARGS."
  (interactive)
  (rustic-compilation-process-live)
  (let* ((command (list (rustic-cargo-bin) rustic-cargo-test-exec-command))
         (c (append command (split-string (if test-args test-args ""))))
         (buf rustic-test-buffer-name)
         (proc rustic-test-process-name)
         (mode 'rustic-cargo-test-mode))
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-test (&optional arg)
  "Run 'cargo test'.

If ARG is not nil, use value as argument and store it in `rustic-test-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-test-arguments'."
  (interactive "P")
  (rustic-cargo-test-run
   (cond (arg
          (setq rustic-test-arguments (read-from-minibuffer "Cargo test arguments: " rustic-default-test-arguments)))
         ((eq major-mode 'rustic-popup-mode)
          (if (> (length rustic-test-arguments) 0)
              rustic-test-arguments
            rustic-default-test-arguments))
         (t
          rustic-default-test-arguments))))

;;;###autoload
(defun rustic-cargo-test-rerun ()
  "Run 'cargo test' with `rustic-test-arguments'."
  (interactive)
  (rustic-cargo-test-run rustic-test-arguments))

;;;###autoload
(defun rustic-cargo-current-test ()
  "Run 'cargo test' for the test near point."
  (interactive)
  (rustic-compilation-process-live)
  (-if-let (test-to-run (setq rustic-test-arguments
                              (rustic-cargo--get-test-target)))
      (rustic-cargo-run-test test-to-run)
    (message "Could not find test at point.")))

;;;###autoload
(defun rustic-cargo-test-dwim ()
  "Run test or mod at point. Otherwise run `rustic-cargo-test'."
  (interactive)
  (if-let (test (or (rustic-cargo--get-current-fn-name)
                    (rustic-cargo--get-current-mod)))
      (rustic-cargo-test)))

(defconst rustic-cargo-mod-regexp
  "^\s*mod\s+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)\s*{")
(defconst rustic-cargo-fn-regexp
  "^\s*\\(?:async\s+\\)?\s*fn\s+\\([^(]+\\)\s*(")

(defun rustic-cargo--get-test-target()
  "Return either a full fn name or a mod name, whatever is closer to the point."
  (let ((mod-cons (rustic-cargo--get-current-mod))
        (fn-cons (rustic-cargo--get-current-fn-name)))
    (cond ((and mod-cons fn-cons)
           ;; both conses contain (location . name)
           (if (> (car mod-cons) (car fn-cons))
               (cdr mod-cons)
             (concat (cdr mod-cons) "::" (cdr fn-cons))))
          (fn-cons (cdr fn-cons))
          (t (cdr mod-cons)))))

(defun rustic-cargo--get-current-mod ()
  "Return cons with location and mod name around point or nil."
  (save-excursion
    (progn
      (goto-char (line-end-position))
      (when-let ((location (search-backward-regexp rustic-cargo-mod-regexp nil t)))
        (cons location (match-string 1))))))

(defun rustic-cargo--get-current-line-fn-name ()
  "Return cons with location and fn name from the current line or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when-let ((location (search-forward-regexp rustic-cargo-fn-regexp (line-end-position) t)))
      (cons location (match-string 1)))))

(defun rustic-cargo--get-current-fn-name ()
  "Return fn name around point or nil."
  (save-excursion
    (or (rustic-cargo--get-current-line-fn-name)
        (progn
          (rustic-beginning-of-function)
          (rustic-cargo--get-current-line-fn-name)))))

;;; Outdated

(defvar rustic-cargo-outdated-process-name "rustic-cargo-outdated-process")

(defvar rustic-cargo-oudated-buffer-name "*cargo-outdated*")

(defvar rustic-cargo-outdated-spinner nil)

(defvar rustic-cargo-outdated-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "m") 'rustic-cargo-menu-mark-unmark)
    (define-key map (kbd "u") 'rustic-cargo-mark-upgrade)
    (define-key map (kbd "U") 'rustic-cargo-mark-all-upgrades)
    (define-key map (kbd "x") 'rustic-cargo-upgrade-execute)
    (define-key map (kbd "r") 'rustic-cargo-reload-outdated)
    (define-key map (kbd "l") 'rustic-cargo-mark-latest-upgrade)
    (define-key map (kbd "L") 'rustic-cargo-mark-all-upgrades-latest)
    (define-key map (kbd "c") 'rustic-compile)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Local keymap for `rustic-cargo-outdated-mode' buffers.")

(define-derived-mode rustic-cargo-outdated-mode tabulated-list-mode "cargo-outdated"
  "Major mode for viewing outdated crates in the current workspace."
  (setq truncate-lines t)
  (setq tabulated-list-format
        `[("Name" 25 nil)
          ("Project" 10 nil)
          ("Compat" 10 nil)
          ("Latest" 10 nil)
          ("Kind" 10 nil)
          ("Platform" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun rustic-cargo-outdated (&optional path)
  "Use 'cargo outdated' to list outdated packages in `tabulated-list-mode'.
Execute process in PATH."
  (interactive)
  (let* ((dir (or path (rustic-buffer-crate)))
         (buf (get-buffer-create rustic-cargo-oudated-buffer-name))
         (default-directory dir)
         (inhibit-read-only t))
    (make-process :name rustic-cargo-outdated-process-name
                  :buffer buf
                  :command `(,(rustic-cargo-bin) "outdated" "--depth" "1")
                  :filter #'rustic-cargo-outdated-filter
                  :sentinel #'rustic-cargo-outdated-sentinel
                  :file-handler t)
    (with-current-buffer buf
      (setq default-directory dir)
      (erase-buffer)
      (rustic-cargo-outdated-mode)
      (rustic-with-spinner rustic-cargo-outdated-spinner
        (make-spinner rustic-spinner-type t 10)
        '(rustic-cargo-outdated-spinner
          (":Executing " (:eval (spinner-print rustic-cargo-outdated-spinner))))
        (spinner-start rustic-cargo-outdated-spinner)))
    (display-buffer buf)))

;;;###autoload
(defun rustic-cargo-reload-outdated ()
  "Update list of outdated packages."
  (interactive)
  (rustic-cargo-outdated default-directory))

(defun rustic-cargo-outdated-filter (proc output)
  "Filter for rustic-cargo-outdated-process."
  (let ((inhibit-read-only t))
    (with-current-buffer (process-buffer proc)
      (insert output))))

(defun rustic-cargo-outdated--skip-to-packages ()
  "Move line forward till we reach the package name."
  (goto-char (point-min))
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (while (not (or (eobp) (s-starts-with? "--" line)))
      (forward-line 1)
      (setf line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (when (s-starts-with? "--" line) (forward-line 1))))

(defun rustic-cargo-outdated-sentinel (proc _output)
  "Sentinel for rustic-cargo-outdated-process."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t)
        (exit-status (process-exit-status proc)))
    (if (zerop exit-status)
        (with-current-buffer buf
          (rustic-cargo-outdated--skip-to-packages)
          (let ((packages (split-string
                           (buffer-substring (point) (point-max)) "\n" t)))
            (erase-buffer)
            (rustic-cargo-outdated-generate-menu packages))
          (pop-to-buffer buf))
      (with-current-buffer buf
        (let ((out (buffer-string)))
          (if (= exit-status 101)
              (rustic-cargo-install-crate-p "outdated")
            (message out))))))
  (rustic-with-spinner rustic-cargo-outdated-spinner nil nil))

(defun rustic-cargo-install-crate-p (crate)
  "Ask whether to install crate CRATE."
  (let ((cmd (format "cargo install cargo-%s" crate)))
    (when (yes-or-no-p (format "Cargo-%s missing. Install ? " crate))
      (async-shell-command cmd (rustic-cargo-bin) "cargo-error"))))

(defun rustic-cargo-outdated-generate-menu (packages)
  "Re-populate the `tabulated-list-entries' with PACKAGES."
  (setq tabulated-list-entries
        (mapcar #'rustic-cargo-outdated-menu-entry packages))
  (tabulated-list-print t))

(defun rustic-cargo-outdated-menu-entry (crate)
  "Return a package entry of CRATE suitable for `tabulated-list-entries'."
  (let* ((fields (split-string crate "\s\s+" ))
         (name (nth 0 fields))
         (project (nth 1 fields))
         (compat (nth 2 fields)))
    (list name `[,name
                 ,project
                 ,(if (when (not (string-match "^-" compat))
                        (version< project compat))
                      (propertize compat 'font-lock-face 'rustic-cargo-outdated)
                    compat)
                 ,(nth 3 fields)
                 ,(nth 4 fields)
                 ,(nth 5 fields)])))

;;;###autoload
(defun rustic-cargo-mark-upgrade ()
  "Mark an upgradable package."
  (interactive)
  (let* ((crate (tabulated-list-get-entry (point)))
         (v (read-from-minibuffer "Update to version: "
                                  (substring-no-properties (elt crate 2))))
         (inhibit-read-only t))
    (when v
      (save-excursion
        (goto-char (line-beginning-position))
        (save-match-data
          (when (search-forward (elt crate 0))
            (replace-match (propertize (elt crate 0)
                                       'font-lock-face
                                       'rustic-cargo-outdated-upgrade)))
          (goto-char (line-beginning-position))
          (when (search-forward (elt crate 1))
            (replace-match (propertize v
                                       'font-lock-face
                                       'rustic-cargo-outdated-upgrade)))))
      (tabulated-list-put-tag "U" t))))

;;;###autoload
(defun rustic-cargo-mark-latest-upgrade ()
  "Mark an upgradable package to the latest available version."
  (interactive)
  (let* ((crate (tabulated-list-get-entry (point)))
         (v (substring-no-properties (elt crate 3)))
         (line-beg (line-beginning-position))
         (inhibit-read-only t))
    (when v
      (save-excursion
        (goto-char line-beg)
        (save-match-data
          (when (search-forward (elt crate 0))
            (replace-match (propertize (elt crate 0)
                                       'font-lock-face
                                       'rustic-cargo-outdated-upgrade)))
          (goto-char (line-beginning-position))
          (when (search-forward (elt crate 1))
            (replace-match (propertize v
                                       'font-lock-face
                                       'rustic-cargo-outdated-upgrade)))))
      (tabulated-list-put-tag "U" t))))

;;;###autoload
(defun rustic-cargo-mark-all-upgrades-latest ()
  "Mark all packages in the Package Menu to latest version."
  (interactive)
  (tabulated-list-print t)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((crate (aref (tabulated-list-get-entry) 0))
             (current-version (aref (tabulated-list-get-entry) 1))
             (latest-version (aref (tabulated-list-get-entry) 3))
             (line-beg (line-beginning-position))
             (replace-highlight-text
              (lambda (text)
                (replace-match (propertize text
                                           'font-lock-face
                                           'rustic-cargo-outdated-upgrade))))
             (inhibit-read-only t))
        (save-match-data
          (when (search-forward crate)
            (funcall replace-highlight-text crate))
          (goto-char line-beg)
          (when (search-forward current-version)
            (funcall replace-highlight-text latest-version)))
        (tabulated-list-put-tag "U")
        (forward-line)))))

;;;###autoload
(defun rustic-cargo-mark-all-upgrades ()
  "Mark all upgradable packages in the Package Menu."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((project (aref (tabulated-list-get-entry) 1))
            (compat (aref (tabulated-list-get-entry) 2)))
        (if (or (string-match "^-" compat)
                (not (version< project compat)))
            (forward-line)
          (tabulated-list-put-tag "U" t))))))

;;;###autoload
(defun rustic-cargo-menu-mark-unmark ()
  "Clear any marks on a package."
  (interactive)
  (tabulated-list-put-tag " " t))

(cl-defstruct rustic-crate name version)

(defun rustic-cargo--outdated-make-crate (crate-line)
  "Create RUSTIC-CRATE struct out of a CRATE-LINE.

The CRATE-LINE is a single line from the `rustic-cargo-oudated-buffer-name'"
  (make-rustic-crate :name (nth 1 crate-line) :version (nth 2 crate-line)))

;;;###autoload
(defun rustic-cargo-upgrade-execute ()
  "Perform marked menu actions."
  (interactive)
  (let ((crates (rustic-cargo--outdated-get-crates (buffer-string))))
    (if crates
        (let ((msg (format "Upgrade %s ?" (mapconcat #'(lambda (x) (rustic-crate-name x)) crates " "))))
          (when (yes-or-no-p msg)
            (rustic-cargo-upgrade-crates crates)))
      (user-error "No operations specified"))))

(defun rustic-cargo--outdated-get-crates (cargo-outdated-buffer-string)
  "Return a list of `rustic-crate' which needs to be updated.

 CARGO-OUTDATED-BUFFER-STRING represents the entire buffer of
`rustic-cargo-oudated-buffer-name'"
  (let* ((lines (s-lines cargo-outdated-buffer-string))
         (new-crates (-filter (lambda (crate) (s-starts-with? "U" crate)) lines))
         (crates (-map (lambda (crate)
                         (rustic-cargo--outdated-make-crate
                          (s-split " " (s-collapse-whitespace crate)))) new-crates)))
    crates))

(defun rustic-cargo-upgrade-crates (crates)
  "Upgrade CRATES."
  (let (upgrade)
    (dolist (crate crates)
      (setq upgrade (concat upgrade (format "%s@%s " (rustic-crate-name crate) (rustic-crate-version crate)))))
    (let ((output (shell-command-to-string (format "cargo upgrade %s" upgrade))))
      (if (string-match "error: no such subcommand:" output)
          (rustic-cargo-install-crate-p "edit")
        (rustic-cargo-reload-outdated)))))

;;; New project

(defun rustic-create-project (project-path is-new &optional bin)
  "Run either 'cargo new' if IS-NEW is non-nil, or 'cargo init' otherwise.
Creates or initializes the directory at the path specified by PROJECT-PATH. If
BIN is not nil, create a binary application, otherwise a library."
  (let* ((cmd (if is-new "new" "init"))
         (bin (if (or bin (y-or-n-p "Create new binary package? "))
                  "--bin"
                "--lib"))
         (new-sentinel (lambda (_process signal)
                         (when (equal signal "finished\n")
                           (message (format "Created new package: %s"
                                            (file-name-base project-path)))
                           (when rustic-cargo-open-new-project
                             (find-file (concat project-path
                                                (if (string= bin "--bin")
                                                    "/src/main.rs"
                                                  "/src/lib.rs")))))))
         (proc (format "rustic-cargo-%s-process" cmd))
         (buf (format "*cargo-%s*" cmd)))
    (make-process :name proc
                  :buffer buf
                  :command (list (rustic-cargo-bin) cmd bin project-path)
                  :sentinel new-sentinel
                  :file-handler t)))

;;;###autoload
(defun rustic-cargo-new (project-path &optional bin)
  "Run 'cargo new' to start a new package in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library."
  (interactive "DProject path: ")
  (rustic-create-project project-path t bin))

;;;###autoload
(defun rustic-cargo-init (project-path &optional bin)
  "Run 'cargo init' to initialize a directory in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library."
  (interactive "DProject path: ")
  (rustic-create-project project-path nil bin))

;;; Run

(defvar rustic-run-process-name "rustic-cargo-run-process"
  "Process name for run processes.")

(defvar rustic-run-buffer-name "*cargo-run*"
  "Buffer name for run buffers.")

(defvar rustic-run-arguments ""
  "Holds arguments for 'cargo run', similar to `compilation-arguments`.")

(defvar rustic-cargo-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap recompile] 'rustic-cargo-run-rerun)
    map)
  "Local keymap for `rustic-cargo-test-mode' buffers.")

(define-derived-mode rustic-cargo-run-mode rustic-compilation-mode "cargo-run"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-run-command (&optional run-args)
  "Start compilation process for 'cargo run' with optional RUN-ARGS."
  (interactive)
  (rustic-compilation-process-live)
  (let* ((command (list (rustic-cargo-bin) rustic-cargo-run-exec-command))
         (c (append command (split-string (if run-args run-args ""))))
         (buf rustic-run-buffer-name)
         (proc rustic-run-process-name)
         (mode 'rustic-cargo-run-mode))
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-run (&optional arg)
  "Run 'cargo run'.

If ARG is not nil, use value as argument and store it in `rustic-run-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-run-arguments'."
  (interactive "P")
  (rustic-cargo-run-command
   (cond (arg
          (setq rustic-run-arguments (read-from-minibuffer "Cargo run arguments: " rustic-run-arguments)))
         ((eq major-mode 'rustic-popup-mode)
          rustic-run-arguments)
         ((rustic--get-run-arguments))
         (t ""))))

;;;###autoload
(defun rustic-cargo-run-rerun ()
  "Run 'cargo run' with `rustic-run-arguments'."
  (interactive)
  (rustic-cargo-run-command rustic-run-arguments))

(defun rustic--get-run-arguments ()
  "Helper utility for getting arguments related to 'examples' directory."
  (if (rustic-cargo-run-get-relative-example-name)
      (concat "--example " (rustic-cargo-run-get-relative-example-name))
    (car compile-history)))

(defun rustic-cargo-run-get-relative-example-name ()
  "Run 'cargo run --example' if current buffer within a 'examples' directory."
  (let* ((buffer-project-root (rustic-buffer-crate))
         (relative-filenames
          (if buffer-project-root
              (split-string (file-relative-name buffer-file-name buffer-project-root) "/") nil)))
    (if (and relative-filenames (string= "examples" (car relative-filenames)))
        (let ((size (length relative-filenames)))
          (cond ((eq size 2) (file-name-sans-extension (nth 1 relative-filenames))) ;; examples/single-example1.rs
                ((> size 2) (car (nthcdr (- size 2) relative-filenames)))           ;; examples/example2/main.rs
                (t nil))) nil)))

;;;###autoload
(defun rustic-run-shell-command (&optional arg)
  "Run an arbitrary shell command using ARG for the current project.
Example: use it to provide an environment variable to your
application like this `env MYVAR=1 cargo run' so that it can read
it at the runtime.  As a byproduct, you can run any shell command
in your project like `pwd'"
  (interactive "P")
  (setq command (read-from-minibuffer "Command to execute: " (car compile-history) nil nil 'compile-history))
  (rustic-run-cargo-command command (list :mode 'rustic-cargo-plainrun-mode)))

;;; Cargo commands

(defun rustic-run-cargo-command (command &optional args)
  "Run the specified COMMAND with cargo."
  (rustic-compilation-process-live)
  (let ((c (if (listp command)
               command
             (split-string command))))
    (rustic-compilation-start c (append (list :no-default-dir t) args))))

;;;###autoload
(defun rustic-cargo-build ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command `(,(rustic-cargo-bin)
                              ,rustic-cargo-build-exec-command
                              ,@(split-string rustic-cargo-build-arguments))
                            (list :clippy-fix t)))

(defvar rustic-clean-arguments nil
  "Holds arguments for 'cargo clean', similar to `compilation-arguments`.")

;;;###autoload
(defun rustic-cargo-clean (&optional arg)
  "Run 'cargo clean' for the current project.

If ARG is not nil, use value as argument and store it in `rustic-clean-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clean-arguments'."
  (interactive "P")
  (rustic-run-cargo-command
   (-filter (lambda (s) (s-present? s))
            (-flatten
             (list (rustic-cargo-bin) "clean"
                   (cond (arg
                          (setq rustic-clean-arguments
                                (s-split " "
                                         (read-from-minibuffer "Cargo clean arguments: "
                                                               (s-join " " rustic-clean-arguments)))))
                         (t rustic-clean-arguments)))))))

;;;###autoload
(defun rustic-cargo-check ()
  "Run 'cargo check' for the current project."
  (interactive)
  (rustic-run-cargo-command `(,(rustic-cargo-bin)
                              ,rustic-cargo-check-exec-command
                              ,@(split-string rustic-cargo-check-arguments))))

;;;###autoload
(defun rustic-cargo-bench ()
  "Run 'cargo bench' for the current project."
  (interactive)
  (rustic-run-cargo-command (list (rustic-cargo-bin) "bench")))

;;;###autoload
(defun rustic-cargo-build-doc ()
  "Build the documentation for the current project."
  (interactive)
  (if (y-or-n-p "Create documentation for dependencies?")
      (rustic-run-cargo-command (list (rustic-cargo-bin) "doc"))
    (rustic-run-cargo-command (list (rustic-cargo-bin) "doc" "--no-deps"))))

;; TODO: buffer with cargo output should be in rustic-compilation-mode
;;;###autoload
(defun rustic-cargo-doc ()
  "Open the documentation for the current project in a browser.
The documentation is built if necessary."
  (interactive)
  (if (y-or-n-p "Open docs for dependencies as well?")
      ;; open docs only works with synchronous process
      (shell-command (format "%s doc --open" (rustic-cargo-bin)))
    (shell-command (format "%s doc --open --no-deps" (rustic-cargo-bin)))))

;;; cargo edit

(defun rustic-cargo-edit-installed-p ()
  "Check if cargo-edit is installed. If not, ask the user if he wants to install it."
  (if (executable-find "cargo-add") t (rustic-cargo-install-crate-p "edit") nil))

;;;###autoload
(defun rustic-cargo-add (&optional arg)
  "Add crate to Cargo.toml using 'cargo add'.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (when (rustic-cargo-edit-installed-p)
    (let* ((command (if arg
                        (read-from-minibuffer "Cargo add command: "
                                              (rustic-cargo-bin) " add ")
                      (concat (rustic-cargo-bin) " add "
                              (read-from-minibuffer "Crate: ")))))
      (rustic-run-cargo-command command))))

(defun rustic-cargo-add-missing-dependencies (&optional arg)
  "Lookup and add missing dependencies to Cargo.toml.
Adds all missing crates by default with latest version using lsp functionality.
Supports both lsp-mode and egot.
Use with 'C-u` to open prompt with missing crates."
  (interactive)
  (when (rustic-cargo-edit-installed-p)
    (let (deps)
      (setq deps
            (cond ((featurep 'lsp-mode)
                   (rustic-cargo-add-missing-dependencies-lsp-mode))
                  ((featurep 'eglot)
                   (rustic-cargo-add-missing-dependencies-eglot))
                  (t
                   nil)))
      (if deps
          (progn
            (when (listp deps)
              (setq deps (mapconcat 'identity  deps " ")))
            (let (d)
              (if current-prefix-arg
                  (setq d (read-from-minibuffer "Add dependencies: " deps))
                (setq d deps))
              (rustic-run-cargo-command (concat (rustic-cargo-bin) " add " d))))
        (message "No missing crates found. Maybe check your lsp server.")))))

(defun rustic-cargo-add-missing-dependencies-lsp-mode ()
  "Return missing dependencies using `lsp-diagnostics'."
  (let* ((diags (gethash (buffer-file-name) (lsp-diagnostics t)))
         (lookup-missing-crates
          (lambda (missing-crates errortable)
            (if (string= "E0432" (gethash "code" errortable))
                (cons (nth 3 (split-string (gethash "message" errortable) "`"))
                      missing-crates)
              missing-crates))))
    (delete-dups (seq-reduce lookup-missing-crates
                             diags
                             '()))))

(defun rustic-cargo-add-missing-dependencies-eglot ()
  "Return missing dependencies by parsing flymake diagnostics buffer."
  (let* ((buf (flymake--diagnostics-buffer-name))
         crates)
    ;; ensure flymake diagnostics buffer exists
    (unless (buffer-live-p buf)
      (let* ((name (flymake--diagnostics-buffer-name))
             (source (current-buffer))
             (target (or (get-buffer name)
                         (with-current-buffer (get-buffer-create name)
                           (flymake-diagnostics-buffer-mode)
                           (current-buffer)))))
        (with-current-buffer target
          (setq flymake--diagnostics-buffer-source source)
          (revert-buffer))))
    (with-current-buffer buf
      (let ((errors (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
        (dolist (s errors)
          (if (string-match-p (regexp-quote "unresolved import") s)
              (push (string-trim (car (reverse (split-string s))) "`" "`" ) crates)))))
    crates))

;;;###autoload
(defun rustic-cargo-rm (&optional arg)
  "Remove crate from Cargo.toml using 'cargo rm'.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (when (rustic-cargo-edit-installed-p)
    (let* ((command (if arg
                        (read-from-minibuffer "Cargo rm command: "
                                              (rustic-cargo-bin) " rm ")
                      (concat (rustic-cargo-bin) " rm "
                              (read-from-minibuffer "Crate: ")))))
      (rustic-run-cargo-command command))))

;;;###autoload
(defun rustic-cargo-upgrade (&optional arg)
  "Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (when (rustic-cargo-edit-installed-p)
    (let* ((command (if arg
                        (read-from-minibuffer "Cargo upgrade command: "
                                              (rustic-cargo-bin) " upgrade ")
                      (concat (rustic-cargo-bin) " upgrade"))))
      (rustic-run-cargo-command command))))

;;;###autoload
(defun rustic-cargo-login (token)
  "Add crates.io API token using `cargo login'.

`TOKEN' the token for interacting with crates.io. Visit [1] for
        how to get one

[1] https://doc.rust-lang.org/cargo/reference/publishing.html#before-your-first-publish"

  (interactive "sAPI token: ")
  (shell-command (format "%s login %s" (rustic-cargo-bin) token)))

;; Install

(defvar rustic-install-process-name "rustic-cargo-install-process"
  "Process name for install processes.")

(defvar rustic-install-buffer-name "*cargo-install*"
  "Buffer name for install buffers.")

(defvar rustic-install-arguments ""
  "Holds arguments for 'cargo install', similar to `compilation-arguments`.
Installs that are executed by `rustic-cargo-current-install' will also be
stored in this variable.")

(defvar rustic-install-project-dir nil
  "Crate directory where rustic install should be done.")

(defvar rustic-cargo-install-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map rustic-compilation-mode-map)
    (define-key map [remap recompile] 'rustic-cargo-install-rerun)
    map)
  "Local keymap for `rustic-cargo-install-mode' buffers.")

(define-derived-mode rustic-cargo-install-mode rustic-compilation-mode "cargo-install"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-install-rerun ()
  "Run 'cargo install' with `rustic-install-arguments'."
  (interactive)
  (rustic-compilation-start rustic-install-arguments
                              (list :buffer rustic-install-buffer-name
                                    :process rustic-install-process-name
                                    :mode 'rustic-cargo-install-mode
                                    :directory rustic-install-project-dir)))
;;;###autoload
(defun rustic-cargo-install (&optional arg)
  "Install rust binary using 'cargo install'.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive)
  (let* ((command (if arg
                      (read-from-minibuffer "Cargo install command: "
                                            (rustic-cargo-bin) " install ")
                    (s-join " " (cons (rustic-cargo-bin) (cons "install" rustic-cargo-default-install-arguments)))))
         (c (s-split " " command))
         (buf rustic-install-buffer-name)
         (proc rustic-install-process-name)
         (mode 'rustic-cargo-install-mode)
         (default-directory (rustic-buffer-crate)))
    (setq rustic-install-arguments c)
    (setq rustic-install-project-dir default-directory)
    (rustic-compilation-start c (list :buffer buf :process proc :mode mode
                                      :directory default-directory))))

(provide 'rustic-cargo)
;;; rustic-cargo.el ends here
