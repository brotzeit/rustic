;;; rustic-lsp.el --- lsp-mode support -*-lexical-binding: t-*-

;;; Code:

(require 'lsp)
(require 'dash)
(require 'ht)

(defcustom rustic-analyzer-command '("ra_lsp_server")
  "Command for calling rust analyzer."
  :type '(repeat (string)))

(defconst rustic-analyzer-notification-handlers
  '(("rust-analyzer/publishDecorations" . (lambda (_w _p)))))

(defconst rustic-analyzer-action-handlers
  '(("rust-analyzer.applySourceChange" .
     (lambda (p) (rustic-analyzer-apply-source-change-command p)))))

(defun rustic-analyzer-uri-filename (text-document)
  (lsp--uri-to-path (gethash "uri" text-document)))

(defun rustic-analyzer-goto-lsp-loc (loc)
  (-let (((&hash "line" "character") loc))
    (goto-line (1+ line))
    (move-to-column character)))

(defun rustic-analyzer-apply-text-document-edit (edit)
  "Like lsp--apply-text-document-edit, but it allows nil version."
  (let* ((ident (gethash "textDocument" edit))
         (filename (rustic-analyzer-uri-filename ident))
         (version (gethash "version" ident)))
    (with-current-buffer (find-file-noselect filename)
      (when (or (not version) (= version (lsp--cur-file-version)))
        (lsp--apply-text-edits (gethash "edits" edit))))))

(defun rustic-analyzer-apply-source-change (data)
  (--each (-> data (ht-get "workspaceEdit") (ht-get "documentChanges"))
    (rustic-analyzer-apply-text-document-edit it))
  (-when-let (cursor-position (ht-get data "cursorPosition"))
    (let ((filename (rustic-analyzer-uri-filename (ht-get cursor-position "textDocument")))
          (position (ht-get cursor-position "position")))
      (find-file filename)
      (rustic-analyzer-goto-lsp-loc position))))

(defun rustic-analyzer-apply-source-change-command (p)
  (let ((data (-> p (ht-get "arguments") (car))))
    (rustic-analyzer-apply-source-change data)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () rustic-analyzer-command))
  :notification-handlers (ht<-alist rustic-analyzer-notification-handlers)
  :action-handlers (ht<-alist rustic-analyzer-action-handlers)
  :major-modes '(rustic-mode)
  :ignore-messages nil
  :server-id 'rust-analyzer))


(provide 'rustic-lsp)
;;; rustic-lsp.el ends here
