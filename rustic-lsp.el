;;; rustic-lsp.el --- lsp-mode support -*-lexical-binding: t-*-

;;; Code:

(require 'lsp)
(require 'dash)
(require 'ht)

(defcustom rustic-analyzer-command '("ra_lsp_server")
  "Command for calling rust analyzer."
  :type '(repeat (string)))

(defconst rustic-analyzer--notification-handlers
  '(("rust-analyzer/publishDecorations" . (lambda (_w _p)))))

(defconst rustic-analyzer--action-handlers
  '(("rustic-analyzer.applySourceChange" .
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

(with-eval-after-load 'company-lsp
  ;; company-lsp provides a snippet handler for rust by default that adds () after function calls, which RA does better
  (setq company-lsp--snippet-functions (assq-delete-all "rust" company-lsp--snippet-functions)))


;;;;;;;;;;;;;;;;
;; Interactive

(defvar rustic-analyzer--last-runnable nil)

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
     (string-join (cons bin args) " ")
     ;; cargo-process-mode is nice, but try to work without it...
     (if (functionp 'cargo-process-mode) 'cargo-process-mode nil)
     (lambda (_) (concat "*" label "*")))
    (setq rustic-analyzer--last-runnable runnable)))

(defun rustic-analyzer-rerun (&optional runnable)
  (interactive (list (or rustic-analyzer--last-runnable
                         (rustic-analyzer--select-runnable))))
(rustic-analyzer-run (or runnable rustic-analyzer--last-runnable)))

(provide 'rustic-lsp)
;;; rustic-lsp.el ends here
