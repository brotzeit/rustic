;;; rustic-lsp.el --- Rust analyzer emacs bindings -*-lexical-binding: t-*-

;;; Commentary:

;; This code was pulled from https://github.com/rust-analyzer/rust-analyzer/tree/master/editors/emacs

;;; Code:

(require 'lsp)
(require 'dash)
(require 'ht)

;; This currently
;;  - sets up rustic-analyzer with emacs-lsp, giving
;;    - code actions
;;    - completion (use company-lsp for proper snippet support)
;;    - imenu support
;;    - on-type formatting
;;    - 'hover' type information & documentation (with lsp-ui)
;;  - implements source changes (for code actions etc.), except for file system changes
;;  - implements joinLines (you need to bind rustic-analyzer-join-lines to a key)
;;  - implements selectionRanges (either bind lsp-extend-selection to a key, or use expand-region)
;;  - provides rustic-analyzer-inlay-hints-mode for inline type hints

;; What's missing:
;;  - file system changes in apply-source-change
;;  - semantic highlighting
;;  - onEnter, parentModule, findMatchingBrace
;;  - runnables
;;  - the debugging commands (syntaxTree and analyzerStatus)
;;  - more

;; Also, there's a problem with company-lsp's caching being too eager, sometimes
;; resulting in outdated completions.

(defconst rustic-analyzer--notification-handlers
  '(("rust-analyzer/publishDecorations" . (lambda (_w _p)))))

(defconst rustic-analyzer--action-handlers
  '(("rust-analyzer.applySourceChange" .
     (lambda (p) (rustic-analyzer--apply-source-change-command p)))))

(defun rustic-analyzer--uri-filename (text-document)
  (lsp--uri-to-path (gethash "uri" text-document)))

(defun rustic-analyzer--goto-lsp-loc (loc)
  (-let (((&hash "line" "character") loc))
    (goto-line (1+ line))
    (move-to-column character)))

(defun rustic-analyzer--apply-text-document-edit (edit)
  "Like lsp--apply-text-document-edit, but it allows nil version."
  (let* ((ident (gethash "textDocument" edit))
         (filename (rustic-analyzer--uri-filename ident))
         (version (gethash "version" ident)))
    (with-current-buffer (find-file-noselect filename)
      (when (or (not version) (= version (lsp--cur-file-version)))
        (lsp--apply-text-edits (gethash "edits" edit))))))

(defun rustic-analyzer--apply-source-change (data)
  ;; TODO fileSystemEdits
  (seq-doseq (it (-> data (ht-get "workspaceEdit") (ht-get "documentChanges")))
    (rustic-analyzer--apply-text-document-edit it))
  (-when-let (cursor-position (ht-get data "cursorPosition"))
    (let ((filename (rustic-analyzer--uri-filename (ht-get cursor-position "textDocument")))
          (position (ht-get cursor-position "position")))
      (find-file filename)
      (rustic-analyzer--goto-lsp-loc position))))

(defun rustic-analyzer--apply-source-change-command (p)
  (let ((data (-> p (ht-get "arguments") (seq-first))))
    (rustic-analyzer--apply-source-change data)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () rustic-analyzer-command))
  :notification-handlers (ht<-alist rustic-analyzer--notification-handlers)
  :action-handlers (ht<-alist rustic-analyzer--action-handlers)
  :major-modes '(rustic-mode)
  :ignore-messages nil
  :server-id 'rustic-analyzer))

(defun rustic-analyzer--initialized? ()
  (when-let ((workspace (lsp-find-workspace 'rustic-analyzer (buffer-file-name))))
    (eq 'initialized (lsp--workspace-status workspace))))

(with-eval-after-load 'company-lsp
  ;; company-lsp provides a snippet handler for rust by default that adds () after function calls, which RA does better
  (setq company-lsp--snippet-functions (cl-delete "rust" company-lsp--snippet-functions :key #'car :test #'equal)))

;; join lines

(defun rustic-analyzer--join-lines-params ()
  "Join lines params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point) (point)))))

(defun rustic-analyzer-join-lines ()
  (interactive)
  (->
   (lsp-send-request (lsp-make-request "rust-analyzer/joinLines"
                                       (rustic-analyzer--join-lines-params)))
   (rustic-analyzer--apply-source-change)))

;; selection ranges

(defun rustic-analyzer--add-er-expansion ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(lsp-extend-selection))))

(with-eval-after-load 'expand-region
  ;; add the expansion for all existing rustic-mode buffers. If expand-region is
  ;; loaded lazily, it might be loaded when the first rust buffer is opened, and
  ;; then it's too late for the hook for that buffer
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq 'rustic-mode major-mode)
        (rustic-analyzer--add-er-expansion))))
  (add-hook 'rustic-mode-hook 'rustic-analyzer--add-er-expansion))

;; runnables
(defvar rustic-analyzer--last-runnable nil)

(defun rustic-analyzer--runnables-params ()
  (list :textDocument (lsp--text-document-identifier)
        :position (lsp--cur-position)))

(defun rustic-analyzer--runnables ()
  (lsp-send-request (lsp-make-request "rust-analyzer/runnables"
                                      (rustic-analyzer--runnables-params))))

(defun rustic-analyzer--select-runnable ()
  (lsp--completing-read
   "Select runnable:"
   (if rustic-analyzer--last-runnable
       (cons rustic-analyzer--last-runnable (rustic-analyzer--runnables))
       (rustic-analyzer--runnables))
   (-lambda ((&hash "label")) label)))

(defun rustic-analyzer-run (runnable)
  (interactive (list (rustic-analyzer--select-runnable)))
  (-let (((&hash "env" "bin" "args" "label") runnable))
    (compilation-start
     (string-join (append (list bin) args '()) " ")
     ;; cargo-process-mode is nice, but try to work without it...
     (if (functionp 'cargo-process-mode) 'cargo-process-mode nil)
     (lambda (_) (concat "*" label "*")))
    (setq rustic-analyzer--last-runnable runnable)))

(defun rustic-analyzer-rerun (&optional runnable)
  (interactive (list (or rustic-analyzer--last-runnable
                         (rustic-analyzer--select-runnable))))
  (rustic-analyzer-run (or runnable rustic-analyzer--last-runnable)))

;; analyzer status buffer
(define-derived-mode rustic-analyzer-status-mode special-mode "Rust-Analyzer-Status"
  "Mode for the rustic-analyzer status buffer.")

(defvar-local rustic-analyzer--status-buffer-workspace nil)

(defun rustic-analyzer-status ()
  "Displays status information for rustic-analyzer."
  (interactive)
  (let* ((workspace (lsp-find-workspace 'rustic-analyzer (buffer-file-name)))
         (buf (get-buffer-create (concat "*rust-analyzer status " (with-lsp-workspace workspace (lsp-workspace-root)) "*"))))
    (with-current-buffer buf
      (rustic-analyzer-status-mode)
      (setq rustic-analyzer--status-buffer-workspace workspace)
      (rustic-analyzer-status-buffer-refresh))
    (pop-to-buffer buf)))

(defun rustic-analyzer-status-buffer-refresh ()
  (interactive)
  (when rustic-analyzer--status-buffer-workspace
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (with-lsp-workspace rustic-analyzer--status-buffer-workspace
                (lsp-send-request (lsp-make-request
                                   "rust-analyzer/analyzerStatus")))))))


(defun rustic-analyzer--syntax-tree-params ()
  "Syntax tree params."
  (list :textDocument (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point-min) (point-max)))))

(defun rustic-analyzer-syntax-tree ()
  "Displays syntax tree for current buffer."
  (interactive)
  (when (eq 'rustic-mode major-mode)
    (let* ((workspace (lsp-find-workspace 'rustic-analyzer (buffer-file-name)))
           (buf (get-buffer-create (concat "*rust-analyzer syntax tree " (with-lsp-workspace workspace (lsp-workspace-root)) "*"))))
      (when workspace
        (let ((parse-result (with-lsp-workspace workspace
                              (lsp-send-request (lsp-make-request
                                                 "rust-analyzer/syntaxTree"
                                                 (rustic-analyzer--syntax-tree-params))))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert parse-result)))
          (pop-to-buffer buf))))))

;; inlay hints
(defun rustic-analyzer--update-inlay-hints (buffer)
  (if (and (rustic-analyzer--initialized?) (eq buffer (current-buffer)))
    (lsp-send-request-async
     (lsp-make-request "rust-analyzer/inlayHints"
                       (list :textDocument (lsp--text-document-identifier)))
     (lambda (res)
       (remove-overlays (point-min) (point-max) 'rustic-analyzer--inlay-hint t)
       (dolist (hint res)
         (-let* (((&hash "range" "label" "kind") hint)
                 ((beg . end) (lsp--range-to-region range))
                 (overlay (make-overlay beg end)))
           (overlay-put overlay 'rustic-analyzer--inlay-hint t)
           (overlay-put overlay 'evaporate t)
           (overlay-put overlay 'after-string (propertize (concat ": " label)
                                                          'font-lock-face 'font-lock-comment-face)))))
     'tick))
  nil)

(defvar-local rustic-analyzer--inlay-hints-timer nil)

(defun rustic-analyzer--inlay-hints-change-handler (&rest rest)
  (when rustic-analyzer--inlay-hints-timer
    (cancel-timer rustic-analyzer--inlay-hints-timer))
  (setq rustic-analyzer--inlay-hints-timer
        (run-with-idle-timer 0.1 nil #'rustic-analyzer--update-inlay-hints (current-buffer))))

(define-minor-mode rustic-analyzer-inlay-hints-mode
  "Mode for showing inlay hints."
  nil nil nil
  (cond
   (rustic-analyzer-inlay-hints-mode
    (rustic-analyzer--update-inlay-hints (current-buffer))
    (add-hook 'lsp-after-initialize-hook #'rustic-analyzer--inlay-hints-change-handler nil t)
    (add-hook 'after-change-functions #'rustic-analyzer--inlay-hints-change-handler nil t))
   (t
    (remove-overlays (point-min) (point-max) 'rustic-analyzer--inlay-hint t)
    (remove-hook 'lsp-after-initialize-hook #'rustic-analyzer--inlay-hints-change-handler t)
    (remove-hook 'after-change-functions #'rustic-analyzer--inlay-hints-change-handler t))))

(provide 'rustic-lsp)
;;; rustic-lsp.el ends here
