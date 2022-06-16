;;; rustic-spellcheck.el --- Spellcheck support -*-lexical-binding: t-*-
;;; Commentary:

;; This implements various functionalities related to cargo spellcheck to
;; show result of expansion

;;; Code:

(require 'rustic-cargo)
(require 'rustic-compile)

(defvar rustic-spellcheck-process-name "rustic-cargo-spellcheck-process"
  "Process name for spellcheck processes.")

(defvar rustic-spellcheck-buffer-name "*cargo-spellcheck*"
  "Buffer name for spellcheck buffers.")

(defvar rustic-spellcheck-arguments ""
  "Holds arguments for 'cargo spellcheck', similar to `compilation-arguments`.")

(defvar rustic-cargo-spellcheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap recompile] 'rustic-cargo-spellcheck-rerun)
    map)
  "Local keymap for `rustic-cargo-spellcheck-mode' buffers.")

(defvar rustic-spellcheck-error
  (let ((err ".*--> ")
        (file "\\(.*.rs\\)")
        (start-line "\\([0-9]+\\)"))
    (let ((re (concat err file ":" start-line)))
      (cons re '(1 2 3))))
  "Create hyperlink in compilation buffers for spellcheck errors.")

(defun rustic-spellcheck-compilation (command &optional args)
  "Start a spellcheck compilation process with COMMAND.

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
         (workspace (rustic-buffer-workspace (plist-get args :no-default-dir)))
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

(define-compilation-mode rustic-cargo-spellcheck-mode "rustic-cargo-spellcheck"
  "Rust spellcheck compilation mode.

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
               (cons 'rustic-spell-error rustic-spellcheck-error))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'rustic-spell-error))

;;;###autoload
(defun rustic-cargo-spellcheck (&optional arg)
  "Run 'cargo spellcheck'.

If ARG is not nil, use value as argument and store it in
`rustic-spellcheck-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-spellcheck-arguments'."
  (interactive "P")
  (rustic-cargo-spellcheck-command
   (cond (arg
          (setq rustic-spellcheck-arguments
                (read-from-minibuffer "Cargo spellcheck arguments: " rustic-spellcheck-arguments)))
         (t ""))))

(defun rustic-cargo-spellcheck-command (&optional spellcheck-args)
  "Start compilation process for 'cargo spellcheck' with optional SPELLCHECK-ARGS."
  (let* ((command (list (rustic-cargo-bin) "spellcheck"))
         (c (append command (split-string (if spellcheck-args spellcheck-args ""))))
         (spellcheck-command (string-join c " "))
         (buf rustic-spellcheck-buffer-name)
         (proc rustic-spellcheck-process-name)
         (mode 'rustic-cargo-spellcheck-mode))
    (rustic-spellcheck-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-spellcheck-rerun ()
  "Run 'cargo spellcheck' with `rustic-spellcheck-arguments'."
  (interactive)
  (rustic-cargo-spellcheck-command rustic-spellcheck-arguments))

(provide 'rustic-spellcheck)
;;; rustic-spellcheck.el ends here
