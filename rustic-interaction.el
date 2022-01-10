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
  (let ((workspace (rustic-buffer-crate t)))
    (if workspace
        (find-file (concat workspace "/Cargo.toml"))
      (message "The current buffer is not inside a rust project!"))))

;;; Defun Motions

;; TODO: check if we should use `rustic-func-item-beg-re' or
;;       `rust-top-item-beg-re' here. It seems there was a reason
;;       that we added an extra regexp for funcs.

(defvar rustic-func-item-beg-re
  (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*\\(?:async\\)?\\s-*"
          (regexp-opt '("fn")))
  "Start of a rust function.")

(defun rustic-beginning-of-function ()
  "Move to beginning of rust function at point."
  (rustic-beginning-of-defun 1))

;; this function only differs from the rust-mode version in applying
;; `rustic-func-item-beg-re'
(defun rustic-beginning-of-defun (&optional arg)
  (interactive "p")
  (let* ((arg (or arg 1))
         (magnitude (abs arg))
         (sign (if (< arg 0) -1 1)))
    ;; If moving forward, don't find the defun we might currently be
    ;; on.
    (when (< sign 0)
      (end-of-line))
    (catch 'done
      (dotimes (_ magnitude)
        ;; Search until we find a match that is not in a string or comment.
        (while (if (re-search-backward (concat "^\\(" rustic-func-item-beg-re "\\)")
                                       nil 'move sign)
                   (rust-in-str-or-cmnt)
                 ;; Did not find it.
                 (throw 'done nil)))))
    t))
;;; _
(provide 'rustic-interaction)
;;; rustic-interaction.el ends here
