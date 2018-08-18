;;; rustic-lsp.el --- lsp-mode support -*-lexical-binding: t-*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'font-lock)
(require 'xref)
(require 'dash)
(require 'markdown-mode)

(defvar lsp-rust--config-options (make-hash-table))
(defvar lsp-rust--diag-counters (make-hash-table))
(defvar lsp-rust--running-progress (make-hash-table))

(defcustom lsp-rust-rls-command '("rustup" "run" "nightly" "rls")
  "The command used to launch the RLS.
This should be a list of strings, the first string being the
executable, and the remaining strings being the arguments to this
executable.
If this variable is nil, lsp-rust will try to use the RLS located
at the environment variable RLS_ROOT, if set."
  :type '(repeat (string)))

(defun rustic-lsp-explain-error-at-point ()
  "Explain the error at point.
The explaination comes from 'rustc --explain=ID'."
  (interactive)
  (unless (memq (bound-and-true-p flycheck-checker) '(lsp-ui lsp))
    (user-error "You need to enable lsp-ui-flycheck"))
  (-if-let* ((current-window (selected-window))
             (id (-> (car (flycheck-overlay-errors-at (point)))
                     (flycheck-error-id))))
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*rustc error*")
         (let ((buffer-read-only nil))
           (erase-buffer)
           (insert (shell-command-to-string (concat "rustc --explain=" id))))
         (if (fboundp 'markdown-view-mode)
             (markdown-view-mode)
           (markdown-mode))
         (setq-local markdown-fontify-code-blocks-natively t)
         (setq-local markdown-fontify-code-block-default-mode 'rustic-mode)
         (setq-local kill-buffer-hook (lambda nil
                                        (quit-restore-window)
                                        (when (window-live-p current-window)
                                          (select-window current-window))))
         (setq header-line-format
               (concat (propertize " rustc" 'face 'error)
                       (propertize " " 'display
                                   `(space :align-to (- right-fringe ,(1+ (length id)))))
                       (propertize id 'face 'error)))
         (markdown-toggle-markup-hiding 1)
         (font-lock-ensure)
         (goto-char 1)
         (current-buffer)))
    (message "explain-error: No error at point")))

(defun rustic-lsp-find-implementations ()
  "List all implementation blocks for a trait, struct, or enum at point."
  (interactive)
  (let* ((impls (lsp--send-request (lsp--make-request
                                    "rustDocument/implementations"
                                    (lsp--text-document-position-params))))
         (items (lsp--locations-to-xref-items impls)))
    (if items
        (xref--show-xrefs items nil)
      (message "No implementation found for: %s" (thing-at-point 'symbol t)))))

(defun rustic-lsp-rls-command ()
  "Return the command used to start the RLS for defining the LSP Rust client."
  (or lsp-rust-rls-command
      (-when-let (rls-root (getenv "RLS_ROOT"))
        `("cargo" "+nightly" "run" "--quiet"
          ,(concat "--manifest-path="
                   (concat
                    (file-name-as-directory (expand-file-name rls-root))
                    "Cargo.toml"))
          "--release"))))

(defun rustic-lsp-get-root ()
  (let (dir)
    (unless
	    (ignore-errors
	      (let* ((output (shell-command-to-string "cargo locate-project"))
		         (js (json-read-from-string output)))
	        (setq dir (cdr (assq 'root js)))))
      (error "Couldn't find root for project at %s" default-directory))
    (file-name-directory dir)))

(define-inline rustic-lsp-as-percent (fraction)
  (inline-quote (format "%d%%" (round (* ,fraction 100)))))

(defconst rustic-lsp-handlers
  '(("window/progress" .
     (lambda (workspace progress)
       (let ((id (gethash "id" progress))
	         (message (gethash "message" progress))
	         (percentage (gethash "percentage" progress))
	         (title (gethash "title" progress))
	         (workspace-progress (gethash workspace lsp-rust--running-progress)))
	     (if (gethash "done" progress)
	         (setq workspace-progress (delete id workspace-progress))
	       (delete-dups workspace-progress)
	       (push id workspace-progress))
	     (puthash workspace workspace-progress lsp-rust--running-progress)
	     (setq lsp-status
	           (if workspace-progress
		           (cond
		            ((numberp percentage) (rustic-lsp-as-percent percentage))
		            (message (format "(%s)" message))
		            (title (format "(%s)" (downcase title))))
		         nil)))))
    ;; From rls-vscode:
    ;; FIXME these are legacy notifications used by RLS ca jan 2018.
    ;; remove once we're certain we've progress on.
    ("rustDocument/diagnosticsBegin" . (lambda (_w _p)))
    ("rustDocument/diagnosticsEnd" .
     (lambda (w _p)
       (when (<= (cl-decf (gethash w lsp-rust--diag-counters 0)) 0)
	     (setq lsp-status nil))))
    ("rustDocument/beginBuild" .
     (lambda (w _p)
       (cl-incf (gethash w lsp-rust--diag-counters 0))
       (setq lsp-status "(building)")))))

(defun rustic-lsp-render-string (str)
  (condition-case nil
      (with-temp-buffer
	    (delay-mode-hooks (rustic-mode))
	    (insert str)
	    (font-lock-ensure)
	    (buffer-string))
    (error str)))

(defun rustic-lsp-initialize-client (client)
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p)))
	      rustic-lsp-handlers)
  (lsp-provide-marked-string-renderer client "rust" #'rustic-lsp-render-string))

(lsp-define-stdio-client lsp-rust "rust" #'rustic-lsp-get-root nil
			             :command-fn #'rustic-lsp-rls-command
			             :initialize #'rustic-lsp-initialize-client)

(defun rust-lsp-set-configuration ()
  (lsp--set-configuration `(:rust ,lsp-rust--config-options)))

(add-hook 'lsp-after-initialize-hook 'rust-lsp-set-configuration)

(defun rustic-lsp-set-config (name option)
  "Set a config option in the rust lsp server."
  (puthash name option lsp-rust--config-options))

(defun rustic-lsp-set-build-lib (build)
  "Enable(t)/Disable(nil) building the lib target."
  (rustic-lsp-set-config "build_lib" build))

(defun rustic-lsp-set-build-bin (build)
  "The bin target to build."
  (rustic-lsp-set-config "build_bin" build))

(defun rustic-lsp-set-cfg-test (val)
  "Enable(t)/Disable(nil) #[cfg(test)]."
  (rustic-lsp-set-config "cfg_test" val))

(defun rustic-lsp-set-goto-def-racer-fallback (val)
  "Enable(t)/Disable(nil) goto-definition should use racer as fallback."
  (rustic-lsp-set-config "goto_def_racer_fallback" val))

(provide 'rustic-lsp)
;;; rustic-lsp.el ends here
