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

(defcustom rustic-default-test-arguments "--workspace --benches --tests --all-features"
  "Default arguments when running 'cargo test'."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-check-arguments "--workspace --benches --tests --all-features"
  "Default arguments when running 'cargo check'."
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
    (define-key map (kbd "g") 'rustic-cargo-test-rerun)
    map)
  "Local keymap for `rustic-cargo-test-mode' buffers.")

(define-derived-mode rustic-cargo-test-mode rustic-compilation-mode "cargo-test"
  :group 'rustic

  ;; TODO: append to rustic-compile-rustflags
  (when rustic-cargo-test-disable-warnings
    (setq-local rustic-compile-rustflags "-Awarnings")))

(defun rustic-cargo-run-test (test)
  "Run TEST which can be a single test or mod name."
  (let* ((c (list (rustic-cargo-bin) "test" test))
         (buf rustic-test-buffer-name)
         (proc rustic-test-process-name)
         (mode 'rustic-cargo-test-mode))
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-test-run (&optional test-args)
  "Start compilation process for 'cargo test' with optional TEST-ARGS."
  (interactive)
  (rustic-compilation-process-live)
  (let* ((command (list (rustic-cargo-bin) "test"))
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

(defun rustic-cargo-outdated-sentinel (proc _output)
  "Sentinel for rustic-cargo-outdated-process."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t)
        (exit-status (process-exit-status proc)))
    (if (zerop exit-status)
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-line 2)
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

;;;###autoload
(defun rustic-cargo-upgrade-execute ()
  "Perform marked menu actions."
  (interactive)
  (let (crates)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((cmd (char-after))
               (crate (tabulated-list-get-entry (point))))
          (when (eq cmd ?U)
            (push crate crates)))
        (forward-line)))
    (if crates
        (let ((msg (format "Upgrade %s ?" (mapconcat #'(lambda (x) (elt x 0)) crates " "))))
          (when (yes-or-no-p msg)
            (rustic-cargo-upgrade-crates crates)))
      (user-error "No operations specified"))))

(defun rustic-cargo-upgrade-crates (crates)
  "Upgrade CRATES."
  (let (upgrade)
    (dolist (crate crates)
      (setq upgrade (concat upgrade (format "%s@%s " (elt crate 0) (elt crate 2)))))
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
    (define-key map (kbd "g") 'rustic-cargo-run-rerun)
    map)
  "Local keymap for `rustic-cargo-test-mode' buffers.")

(define-derived-mode rustic-cargo-run-mode rustic-compilation-mode "cargo-run"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-run-command (&optional run-args)
  "Start compilation process for 'cargo run' with optional RUN-ARGS."
  (interactive)
  (rustic-compilation-process-live)
  (let* ((command (list (rustic-cargo-bin) "run"))
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
         ((rustic--get-run-arguments))
         ((eq major-mode 'rustic-popup-mode)
          rustic-run-arguments)
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
    (rustic-compilation-start c args)))

;;;###autoload
(defun rustic-cargo-build ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command (list (rustic-cargo-bin) "build")
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
                              "check"
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
    (rustic-run-cargo-command (list (rustic-cargo-bin) "doc --no-deps"))))

;; TODO: buffer with cargo output should be in rustic-compilation-mode
;;;###autoload
(defun rustic-cargo-doc ()
  "Open the documentation for the current project in a browser.
The documentation is built if necessary."
  (interactive)
  (if (y-or-n-p "Open docs for dependencies as well?")
      ;; open docs only works with synchronous process
      (shell-command (list (rustic-cargo-bin) "doc --open"))
    (shell-command (list (rustic-cargo-bin) "doc --open --no-deps"))))

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

(provide 'rustic-cargo)
;;; rustic-cargo.el ends here
