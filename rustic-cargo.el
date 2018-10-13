;;; rustic-cargo.el --- Cargo based commands -*-lexical-binding: t-*-

;;; Code:

(require 'tabulated-list)
(require 'magit)

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rustic-cargo-bin "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'rustic-cargo)


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

(define-derived-mode rustic-cargo-test-mode rustic-compilation-mode "cargo-test"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-test ()
  "Run 'cargo test'."
  (interactive)
  (let ((command (list rustic-cargo-bin "test"))
        (buf rustic-test-buffer-name)
        (proc rustic-test-process-name)
        (mode 'rustic-cargo-test-mode))
    (rustic-compilation-process-live)
    (rustic-compilation-start command
                              :buffer buf
                              :process proc
                              :mode mode)))


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
  (let ((project (aref (tabulated-list-get-entry) 1))
        (compat (aref (tabulated-list-get-entry) 2)))
    (unless (or (string-match "^-" compat)
                (not (version< project compat)))
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
        (let ((cmd (char-after))
              (crate (tabulated-list-get-id)))
          (when (eq cmd ?U)
            (push (substring-no-properties crate) crates)))
        (forward-line)))
    (if crates
        (let ((msg (format "Upgrade %s ?" (mapconcat 'identity crates " "))))
          (when (yes-or-no-p msg)
            (rustic-cargo-upgrade-crates crates)))
      (user-error "No operations specified"))))

(defun rustic-cargo-upgrade-crates (crates)
  "Upgrade crates CRATES."
  (let (upgrade
        update)
    (dolist (crate crates)
      (setq upgrade (concat upgrade (format " -d %s" crate)))
      (setq update (concat update (format " -p %s" crate))))
    (let ((output (shell-command-to-string (format "cargo upgrade %s" upgrade))))
      (if (string-match "error: no such subcommand:" output)
          (rustic-cargo-install-crate "edit")
        (and (shell-command-to-string (format "cargo update %s" update))
             (rustic-cargo-reload-outdated))))))


;;;;;;;;;;;;
;; Spinner

(eval-and-compile (require 'spinner))
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
  `(when rustic-babel-display-spinner
     (when (spinner-p ,spinner)
       (spinner-stop ,spinner))
     (setq ,spinner ,val)
     (setq mode-line-process ,mode-line)
     ,@body))


;;;;;;;;;;
;; Popup

(defcustom rustic-cargo-popup-commands
  '((?b "Build"    rustic-cargo-build)
    (?f "Fmt"      rustic-cargo-fmt)
    (?r "Run"      rustic-cargo-run)
    (?c "Clippy"   rustic-cargo-clippy)
    (?t "Test"     rustic-cargo-test)
    (?o "Outdated" rustic-cargo-outdated))
  "Commands for `rustic-cargo-popup'."
  :type 'list
  :group 'rustic-cargo)

(defcustom rustic-cargo-popup-default-action 'rustic-cargo-build
  "Default action for `rustic-cargo-popup'."
  :type 'function
  :group 'rustic-cargo)

(defcustom rustic-cargo-popup-max-columns 2
  "Maximal number of columns in `rustic-cargo-popup'."
  :type 'integer
  :group 'rustic-cargo)

(magit-define-popup rustic-cargo-popup
  "Popup console for cargo commands."
  :actions rustic-cargo-popup-commands
  :default-action rustic-cargo-popup-default-action
  :max-action-columns rustic-cargo-popup-max-columns)


;;;;;;;;;;;;;;;;
;; Interactive

(defun rustic-run-cargo-command (command)
  (rustic-compilation-process-live)
  (rustic-compilation-start (split-string command)))

;;;###autoload
(defun rustic-cargo-build ()
  (interactive)
  (rustic-run-cargo-command "cargo build"))

;;;###autoload
(defun rustic-cargo-run ()
  (interactive)
  (rustic-run-cargo-command "cargo run"))

;;;###autoload
(defun rustic-cargo-clean ()
  (interactive)
  (rustic-run-cargo-command "cargo clean"))

;;;###autoload
(defun rustic-cargo-check ()
  (interactive)
  (rustic-run-cargo-command "cargo check"))

;;;###autoload
(defun rustic-cargo-bench ()
  (interactive)
  (rustic-run-cargo-command "cargo bench"))

;;;###autoload
(defun rustic-cargo-new ()
  (interactive)
  (rustic-run-cargo-command "cargo new"))

(provide 'rustic-cargo)
;;; rustic-cargo.el ends here
