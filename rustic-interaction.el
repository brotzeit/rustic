;;; rustic-interaction.el --- Common interactive functions -*-lexical-binding: t-*-

;;; Code:

(require 'newcomment)

(require 'rustic)

;;; Miscellaneous

(defun rustic-docstring-dwim ()
  "Use `comment-dwim' to make a docstring."
  (interactive)
  (let ((comment-start "/// "))
    (call-interactively 'comment-dwim)))

;;;###autoload
(defun rustic-open-dependency-file ()
  "Open the 'Cargo.toml' file at the project root if the current buffer is
visiting a project."
  (interactive)
  (let ((workspace (rustic-buffer-workspace t)))
    (if workspace
        (find-file (concat workspace "/Cargo.toml"))
      (message "The current buffer is not inside a rust project!"))))

;;; Defun Motions

(defvar rustic-func-item-beg-re
  (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*\\(?:async\\)?\\s-*"
          (regexp-opt '("fn")))
  "Start of a rust function.")

(defun rustic-beginning-of-function ()
  "Move to beginning of rust function at point."
  (rustic-beginning-of-defun nil rustic-func-item-beg-re))

;;; _
(provide 'rustic-interaction)
;;; rustic-interaction.el ends here
