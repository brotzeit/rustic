;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Commentary:

;; Facilities for rust tools like rustfmt and clippy.

;;; Code:

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rustic-format-on-save nil
  "Format future rust buffers before saving using rustfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

(defcustom rustic-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rustic)

(defcustom rustic-cargo-bin "cargo"
  "Path to cargo executable.")

(defcustom rustic-format-display-method 'pop-to-buffer
  "Default function used for displaying rustfmt buffer."
  :type 'function)

(defcustom rustic-playpen-url-format "https://play.rustic-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen"
  :type 'string
  :group 'rustic)

(defcustom rustic-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission"
  :type 'string
  :group 'rustic)


;;;;;;;;;;;;
;; Rustfmt 

(defvar rustic-format-process-name "rustic-rustfmt-process"
  "Process name for rustfmt processes.")

(defvar rustic-format-buffer-name "*rustfmt*"
  "Buffer name for rustfmt process buffers.")

(defvar rustic-format-file-name nil
  "Holds last file formatted by `rustic-format-start-process'.")

(defvar rustic-save-pos nil)

(defun rustic-format-start-process (buffer string sentinel)
  "Start a new rustfmt process."
  (let* ((err-buf (get-buffer-create rustic-format-buffer-name))
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list (format "TERM=%s" "ansi"))
                               process-environment))
         (inhibit-read-only t)
         (dir (rustic-buffer-workspace)))
    (setq next-error-last-buffer buffer)
    (with-current-buffer err-buf
      (setq-local default-directory dir)
      (erase-buffer)
      (rustic-format-mode))
    (setq rustic-format-file-name (buffer-file-name buffer))
    (setq rustic-save-pos (point))
    (let ((proc (make-process :name rustic-format-process-name
                              :buffer err-buf
                              :command `(,rustic-rustfmt-bin)
                              :filter #'rustic-compile-filter
                              :sentinel sentinel)))
      (while (not (process-live-p proc))
        (sleep-for 0.01))
      (process-send-string proc string)
      (process-send-eof proc))))

(defun rustic-format-sentinel (proc output)
  "Sentinel for rustfmt processes."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer proc-buffer
      (if (string-match-p "^finished" output)
          (let ((file-buffer (get-file-buffer rustic-format-file-name)))
            (copy-to-buffer file-buffer (point-min) (point-max))
            (with-current-buffer file-buffer
              (goto-char rustic-save-pos))
            (kill-buffer proc-buffer)
            (message "Formatted buffer with rustfmt."))
        (goto-char (point-min))
        (save-excursion
          (save-match-data
            (when (search-forward "<stdin>" nil t)
              (replace-match rustic-format-file-name)))
          (funcall rustic-format-display-method proc-buffer)
          (message "Rustfmt error."))))))

(define-derived-mode rustic-format-mode rustic-compilation-mode "rustfmt"
  :group 'rustic)

(define-derived-mode rustic-cargo-fmt-mode rustic-compilation-mode "cargo-fmt"
  :group 'rustic)

;;;###autoload
(defun rustic-format--enable-format-on-save ()
  "Enable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rustic-format-on-save t))

;;;###autoload
(defun rustic-format--disable-format-on-save ()
  "Disable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rustic-format-on-save nil))

;;;###autoload
(defun rustic-cargo-fmt ()
  (interactive)
  (let ((command (list rustic-cargo-bin "fmt"))
        (buffer-name rustic-format-buffer-name)
        (proc-name rustic-format-process-name)
        (mode 'rustic-cargo-fmt-mode)
        (dir (rustic-buffer-workspace))
        (sentinel #'(lambda (proc output)
                      (let ((proc-buffer (process-buffer proc))
                            (inhibit-read-only t))
                        (with-current-buffer proc-buffer
                          (when (string-match-p "^finished" output)
                            (kill-buffer proc-buffer)
                            (message "Workspace formatted with cargo-fmt.")))))))
    (rustic-compilation-process-live)
    (rustic-compile-start-process command buffer-name proc-name mode dir sentinel)))

(defun rustic-format-buffer ()
  "Format the current buffer using rustfmt."
  (interactive)
  (rustic-format-start-process (current-buffer) (buffer-string) 'rustic-format-sentinel))


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
        (buffer-name rustic-clippy-buffer-name)
        (proc-name rustic-clippy-process-name)
        (mode 'rustic-cargo-clippy-mode)
        (root (rustic-buffer-workspace)))
    (rustic-compilation-process-live)
    (rustic-compile-start-process command buffer-name proc-name mode root)))


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
  "Run `cargo test'."
  (interactive)
  (let ((command (list rustic-cargo-bin "test"))
        (buffer-name rustic-test-buffer-name)
        (proc-name rustic-test-process-name)
        (mode 'rustic-cargo-test-mode)
        (root (rustic-buffer-workspace)))
    (rustic-compilation-process-live)
    (rustic-compile-start-process command buffer-name proc-name mode root)))

;;;;;;;;;;;;;
;; Outdated

(defcustom rustic-cargo-outdated-face "red"
  "Face for upgradeable crates."
  :type 'face
  :group 'rustic)

(defvar rustic-cargo-outdated-process-name "rustic-cargo-outdated-process")

(defvar rustic-cargo-oudated-buffer-name "*cargo-outdated*")

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

(defun rustic-cargo-list-outdated (&optional path)
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
      (setq-local default-directory dir)
      (erase-buffer))))

(defun rustic-cargo-reload-outdated ()
  (interactive)
  (rustic-cargo-list-outdated default-directory))

(defun rustic-cargo-outdated-filter (proc output)
  (let ((inhibit-read-only t))
    (with-current-buffer (process-buffer proc)
      (insert output))))

(defun rustic-cargo-outdated-sentinel (proc output)
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (zerop (process-exit-status proc))
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-line 2)
          (rustic-cargo-outdated-generate-menu (buffer-substring (point) (point-max)) buf))
      (with-current-buffer buf
        (message (buffer-string))))))

(defun rustic-cargo-outdated-generate-menu (output buf)
  (let ((inhibit-read-only t))
    (with-current-buffer buf
      (rustic-cargo-outdated-mode)
      (erase-buffer)
      (goto-char (point-min))
      (setq tabulated-list-entries
            (mapcar #'rustic-cargo-outdated-menu-entry (split-string output "\n" t)))
      (tabulated-list-print t)
      (pop-to-buffer buf))))

(defun rustic-cargo-outdated-menu-entry (crate)
  (let* ((fields (split-string crate "\s\s+" ))
         (name (nth 0 fields))
         (project (nth 1 fields))
         (compat (nth 2 fields)))
    (list name `[,name
                 ,project
                 ,(if (when (not (string= "---" compat))
                          (version< project compat))
                      (propertize compat 'font-lock-face `(:foreground ,rustic-cargo-outdated-face))
                    compat)
                 ,(nth 3 fields)
                 ,(nth 4 fields)
                 ,(nth 5 fields)])))

(defun rustic-cargo-mark-upgrade ()
  "Mark an upgradable package."
  (interactive)
  (let ((project (aref (tabulated-list-get-entry) 1))
        (compat (aref (tabulated-list-get-entry) 2)))
    (unless (or (string= "---" compat)
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
        (if (or (string= "---" compat)
                (not (version< project compat)))
            (forward-line)
          (tabulated-list-put-tag "U" t))))))

(defun rustic-cargo-menu-mark-unmark ()
  "Clear any marks on a package."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun rustic-cargo-upgrade-execute ()
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
            (rustic-cargo-upgrade-crates crates)
            (when (yes-or-no-p "Run rustic-compile ? ")
              (call-interactively 'rustic-compile))))
      (user-error "No operations specified"))))

(defun rustic-cargo-upgrade-crates (crates)
  (let (cmd)
    (dolist (crate crates)
      (setq cmd (concat cmd (format " -d %s" crate))))
    (shell-command-to-string (format "cargo upgrade %s" cmd))))


;;;;;;;;;;;;;;;;
;; Interactive

;;;###autoload
(defun rustic-playpen (begin end)
  "Create a shareable URL for the contents of the current region, 
src-block or buffer on the Rust playpen."
  (interactive "r")
  (let (data)
    (cond 
     ((region-active-p)
      (setq data (buffer-substring begin end)))
     ((org-in-src-block-p)
      (setq data (org-element-property :value (org-element-at-point))))
     (t
      (setq data (buffer-substring (point-min) (point-max)))))
    (let* ((escaped-data (url-hexify-string data))
           (escaped-playpen-url (url-hexify-string (format rust-playpen-url-format escaped-data))))
      (if (> (length escaped-playpen-url) 5000)
          (error "encoded playpen data exceeds 5000 character limit (length %s)"
                 (length escaped-playpen-url))
        (let ((shortener-url (format rust-shortener-url-format escaped-playpen-url))
              (url-request-method "POST"))
          (url-retrieve shortener-url
                        (lambda (state)
                          ;; filter out the headers etc. included at the
                          ;; start of the buffer: the relevant text
                          ;; (shortened url or error message) is exactly
                          ;; the last line.
                          (goto-char (point-max))
                          (let ((last-line (thing-at-point 'line t))
                                (err (plist-get state :error)))
                            (kill-buffer)
                            (if err
                                (error "failed to shorten playpen url: %s" last-line)
                              (let ((URL (read-from-minibuffer "Playpen URL: " last-line)))
                                (browse-url URL)))))))))))

;;;###autoload
(defun rustic-cargo-build ()
  (interactive)
  (call-interactively 'rustic-compile "cargo build"))

(provide 'rustic-util)
;;; rustic-util.el ends here
