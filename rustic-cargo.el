;;; rustic-cargo.el --- Cargo based commands -*-lexical-binding: t-*-

;;; Code:

(require 'tabulated-list)
(require 'spinner)

(require 'rustic-compile)

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rustic-cargo-bin "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-cargo-open-new-project t
  "If t then any project created with cargo-new will be opened automatically.
If nil then the project is simply created."
  :type 'boolean
  :group 'rustic-cargo)

(defface rustic-cargo-outdated-upgrade-face
  '((t (:foreground "LightSeaGreen")))
  "Face used for crates marked for upgrade.")


;;;;;;;;;;;;
;; Spinner

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
                         (repeat :inline t string)))
  :group 'rustic-babel)

(defmacro rustic-with-spinner (spinner val mode-line &rest body)
  (declare (indent defun))
  `(when rustic-display-spinner
     (when (spinner-p ,spinner)
       (spinner-stop ,spinner))
     (setq ,spinner ,val)
     (setq mode-line-process ,mode-line)
     ,@body))


;;;;;;;;;;;
;; Clippy

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
    (rustic-compilation-start command
                              :buffer buf
                              :process proc
                              :mode mode)))


;;;;;;;;;
;; Test

(defvar rustic-test-process-name "rustic-cargo-test-process"
  "Process name for test processes.")

(defvar rustic-test-buffer-name "*cargo-test*"
  "Buffer name for test buffers.")

(defvar rustic-test-arguments ""
  "Holds arguments for 'cargo test', similiar to `compilation-arguments`.")

(define-derived-mode rustic-cargo-test-mode rustic-compilation-mode "cargo-test"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-test (&optional arg)
  "Run 'cargo test'.

If ARG is not nil, use value as argument and store it in `rustic-test-arguments'"
  (interactive "P")
  (let* ((command (list rustic-cargo-bin "test"))
         (c (if arg
                (setq rustic-test-arguments
                      (concat command " "
                              (read-from-minibuffer "Cargo test arguments: ")))
              command))
         (buf rustic-test-buffer-name)
         (proc rustic-test-process-name)
         (mode 'rustic-cargo-test-mode))
    (rustic-compilation-process-live)
    (rustic-compilation-start c
                              :buffer buf
                              :process proc
                              :mode mode)))

;;;###autoload
(defun rustic-cargo-current-test ()
  "Run 'cargo test' for the test near point."
  (interactive)
  (rustic-compilation-process-live)
  (rustic-compilation-start (list rustic-cargo-bin "test" (rustic-cargo--get-current-fn-fullname))
                            :buffer rustic-test-buffer-name
                            :process rustic-test-process-name
                            :mode 'rustic-cargo-test-mode))

(defun rustic-cargo--get-current-fn-fullname()
  "Return full name of the fn around point including module name if any."
  (let ((mod (rustic-cargo--get-current-mod))
        (fn (rustic-cargo--get-current-fn-name)))
    (if mod
        (concat mod "::" fn)
      fn)))

(defconst rustic-cargo-mod-regexp "^[[:space:]]*mod[[:space:]]+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)[[:space:]]*{")
(defconst rustic-cargo-fn-regexp "^[[:space:]]*fn[[:space:]]+\\([^(]+\\)[[:space:]]*(")

(defun rustic-cargo--get-current-mod ()
  "Return mod name around pount or nil."
  (save-excursion
    (when (search-backward-regexp rustic-cargo-mod-regexp nil t)
      (match-string 1))))

(defun rustic-cargo--get-current-line-fn-name()
  "Return fn name from the current line or nil."
  (save-excursion
    (beginning-of-line)
    (when (search-forward-regexp rustic-cargo-fn-regexp (line-end-position) t)
      (match-string 1))))

(defun rustic-cargo--get-current-fn-name()
  "Return fn name around point or nil."
  (save-excursion
    (or (rustic-cargo--get-current-line-fn-name)
        (progn
          (rustic-beginning-of-defun)
          (rustic-cargo--get-current-line-fn-name)))))

;;;;;;;;;;;;;
;; Outdated

(defcustom rustic-cargo-outdated-face "red"
  "Face for upgradeable crates."
  :type 'face
  :group 'rustic)

(defvar rustic-cargo-outdated-process-name "rustic-cargo-outdated-process")

(defvar rustic-cargo-oudated-buffer-name "*cargo-outdated*")

(defvar rustic-outdated-spinner nil)

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
      (rustic-with-spinner rustic-outdated-spinner
        (make-spinner rustic-spinner-type t 10)
        '(rustic-outdated-spinner (":Executing " (:eval (spinner-print rustic-outdated-spinner))))
        (spinner-start rustic-outdated-spinner)))
    (display-buffer buf)))

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
  (rustic-with-spinner rustic-outdated-spinner nil nil))

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
                      (propertize compat
                                  'font-lock-face
                                  `(:foreground ,rustic-cargo-outdated-face))
                    compat)
                 ,(nth 3 fields)
                 ,(nth 4 fields)
                 ,(nth 5 fields)])))

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
                                       'rustic-cargo-outdated-upgrade-face)))
          (goto-char (line-beginning-position))
          (when (search-forward (elt crate 1))
            (replace-match (propertize v
                                       'font-lock-face
                                       'rustic-cargo-outdated-upgrade-face)))))
      (tabulated-list-put-tag "U" t))))

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

(defun rustic-cargo-menu-mark-unmark ()
  "Clear any marks on a package."
  (interactive)
  (tabulated-list-put-tag " " t))

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

;;;;;;;;;;;;;;;;
;; New project

;;;###autoload
(defun rustic-cargo-new (project-path &optional bin)
  "Run 'cargo new' to start a new package in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library."
  (interactive "DProject path: ")
  (let ((bin (if (or bin (y-or-n-p "Create new binary package? "))
                 "--bin"
               "--lib"))
        (new-sentinel (lambda (process signal)
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

;;;;;;;;;;;;;;;;
;; Interactive

(defun rustic-run-cargo-command (command)
  (rustic-compilation-process-live)
  (rustic-compilation-start (split-string command)))

;;;###autoload
(defun rustic-cargo-build ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo build"))

;;;###autoload
(defun rustic-cargo-run ()
  "Run 'cargo run' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo run"))

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


(provide 'rustic-cargo)
;;; rustic-cargo.el ends here
