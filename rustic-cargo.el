;;; rustic-cargo.el --- Cargo based commands -*-lexical-binding: t-*-

;;; Code:

(require 'dash)
(require 'tabulated-list)

(require 'rustic-common)
(require 'rustic-interaction) ; for rustic-beginning-of-function

;;; Customization

(defcustom rustic-cargo-bin "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-open-new-project t
  "If t then any project created with cargo-new will be opened automatically.
If nil then the project is simply created."
  :type 'boolean
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

;;; Clippy

(defvar rustic-clippy-process-name "rustic-cargo-clippy-process"
  "Process name for clippy processes.")

(defvar rustic-clippy-buffer-name "*cargo-clippy*"
  "Buffer name for clippy buffers.")

(define-derived-mode rustic-cargo-clippy-mode rustic-compilation-mode "cargo-clippy"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-clippy ()
  "Run `cargo clippy'."
  (interactive)
  (let ((command (list rustic-cargo-bin "clippy"))
        (buf rustic-clippy-buffer-name)
        (proc rustic-clippy-process-name)
        (mode 'rustic-cargo-clippy-mode))
    (rustic-compilation-process-live)
    (rustic-compilation command
                        (list
                         :buffer buf
                         :process proc
                         :mode mode))))

;;; Test

(defvar rustic-test-process-name "rustic-cargo-test-process"
  "Process name for test processes.")

(defvar rustic-test-buffer-name "*cargo-test*"
  "Buffer name for test buffers.")

(defvar rustic-test-arguments ""
  "Holds arguments for 'cargo test', similar to `compilation-arguments`.")

(define-derived-mode rustic-cargo-test-mode rustic-compilation-mode "cargo-test"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-test-run (&optional test-args)
  "Start compilation process for 'cargo test' with optional TEST-ARGS."
  (interactive)
  (rustic-compilation-process-live)
  (let* ((command (list rustic-cargo-bin "test"))
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
          (setq rustic-test-arguments (read-from-minibuffer "Cargo test arguments: ")))
         ((eq major-mode 'rustic-popup-mode)
          rustic-test-arguments)
         (t ""))))

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
  (-if-let (func-name (rustic-cargo--get-current-fn-fullname))
      (let* ((command (list rustic-cargo-bin "test" func-name))
             (buf rustic-test-buffer-name)
             (proc rustic-test-process-name)
             (mode 'rustic-cargo-test-mode))
        (rustic-compilation command (list :buffer buf :process proc :mode mode)))
    (message "Could not find test at point.")))

(defconst rustic-cargo-mod-regexp
  "^\s*mod\s+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)\s*{")
(defconst rustic-cargo-fn-regexp
  "^\s*\\(?:async\\)?\s+fn\s+\\([^(]+\\)\s*(")

(defun rustic-cargo--get-current-fn-fullname()
  "Return full name of the fn around point including module name if any."
  (let ((mod (rustic-cargo--get-current-mod))
        (fn (rustic-cargo--get-current-fn-name)))
    (if mod
        (concat mod "::" fn)
      fn)))

(defun rustic-cargo--get-current-mod ()
  "Return mod name around point or nil."
  (save-excursion
    (when (search-backward-regexp rustic-cargo-mod-regexp nil t)
      (match-string 1))))

(defun rustic-cargo--get-current-line-fn-name()
  "Return fn name from the current line or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (search-forward-regexp rustic-cargo-fn-regexp (line-end-position) t)
      (match-string 1))))

(defun rustic-cargo--get-current-fn-name()
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
  (let* ((dir (or path (rustic-buffer-workspace)))
         (buf (get-buffer-create rustic-cargo-oudated-buffer-name))
         (default-directory dir)
         (inhibit-read-only t))
    (make-process :name rustic-cargo-outdated-process-name
                  :buffer buf
                  :command '("cargo" "outdated" "--depth" "1")
                  :filter #'rustic-cargo-outdated-filter
                  :sentinel #'rustic-cargo-outdated-sentinel)
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
      (async-shell-command cmd "cargo" "cargo-error"))))

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

;;;###autoload
(defun rustic-cargo-new (project-path &optional bin)
  "Run 'cargo new' to start a new package in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library."
  (interactive "DProject path: ")
  (let ((bin (if (or bin (y-or-n-p "Create new binary package? "))
                 "--bin"
               "--lib"))
        (new-sentinel (lambda (_process signal)
                        (when (equal signal "finished\n")
                          (message (format "Created new package: %s"
                                           (file-name-base project-path)))
                          (if rustic-cargo-open-new-project
                              (find-file (concat project-path "/src/main.rs"))))))
        (proc "rustic-cargo-new-process")
        (buf "*cargo-new*"))
    (make-process :name proc
                  :buffer buf
                  :command (list rustic-cargo-bin "new" bin project-path)
                  :sentinel new-sentinel)))

;;; Cargo commands

(defun rustic-run-cargo-command (command &optional args)
  "Run the specified COMMAND with cargo."
  (rustic-compilation-process-live)
  (rustic-compilation-start (split-string command) args))

;;;###autoload
(defun rustic-cargo-build ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo build"))

;;;###autoload
(defun rustic-cargo-run (&optional arg)
  "Run 'cargo run' for the current project.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (let* ((command (if arg
                      (read-from-minibuffer "Cargo run command: " "cargo run ")
                    (concat rustic-cargo-bin " run "
                            (read-from-minibuffer
                             "Run arguments: "
                             (car compile-history)
                             nil nil
                             'compile-history)))))
    (rustic-run-cargo-command command (list :mode 'rustic-cargo-run-mode))))

(define-derived-mode rustic-cargo-run-mode rustic-compilation-mode "Cargo run"
  "Mode for 'cargo run' that derives from `rustic-compilation-mode', but uses
the keymap of `comint-mode' so user input is possible."
  (buffer-disable-undo)
  (setq buffer-read-only nil)
  (use-local-map comint-mode-map))

;;;###autoload
(defun rustic-cargo-clean ()
  "Run 'cargo clean' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo clean"))

;;;###autoload
(defun rustic-cargo-check ()
  "Run 'cargo check' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo check"))

;;;###autoload
(defun rustic-cargo-bench ()
  "Run 'cargo bench' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo bench"))

;;;###autoload
(defun rustic-cargo-build-doc ()
  "Build the documentation for the current project."
  (interactive)
  (if (y-or-n-p "Create documentation for dependencies?")
      (rustic-run-cargo-command "cargo doc")
    (rustic-run-cargo-command "cargo doc --no-deps")))

;; TODO: buffer with cargo output should be in rustic-compilation-mode
;;;###autoload
(defun rustic-cargo-doc ()
  "Open the documentation for the current project in a browser.
The documentation is built if necessary."
  (interactive)
  (if (y-or-n-p "Open docs for dependencies as well?")
      ;; open docs only works with synchronous process
      (shell-command "cargo doc --open")
    (shell-command "cargo doc --open --no-deps")))

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
                        (read-from-minibuffer "Cargo add command: " "cargo add ")
                      (concat "cargo add " (read-from-minibuffer "Crate: ")))))
      (rustic-run-cargo-command command))))

;;;###autoload
(defun rustic-cargo-rm (&optional arg)
  "Remove crate from Cargo.toml using 'cargo rm'.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (when (rustic-cargo-edit-installed-p)
    (let* ((command (if arg
                        (read-from-minibuffer "Cargo rm command: " "cargo rm ")
                      (concat "cargo rm " (read-from-minibuffer "Crate: ")))))
      (rustic-run-cargo-command command))))

;;;###autoload
(defun rustic-cargo-upgrade (&optional arg)
  "Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (when (rustic-cargo-edit-installed-p)
    (let* ((command (if arg
                        (read-from-minibuffer "Cargo upgrade command: " "cargo upgrade ")
                      (concat "cargo upgrade"))))
      (rustic-run-cargo-command command))))

(provide 'rustic-cargo)
;;; rustic-cargo.el ends here
