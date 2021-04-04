;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Commentary:

;; Contains functions for rust tools like rustfmt and RLS.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'package)

(require 'rustic-common)
(require 'rustic-cargo)

;;; Customization

(defcustom rustic-list-project-buffers-function
  (if (fboundp 'projectile-project-buffers)
      'projectile-project-buffers
    'rustic-project-buffer-list)
  "Function used to list buffers belonging to current project."
  :type '(choice (const projectile-project-buffers)
                 (const rustic-project-buffer-list)
                 function)
  :group 'rustic)

;;; Interactive

;;;###autoload
(defun rustic-open-dependency-file ()
  "Open the 'Cargo.toml' file at the project root if the current buffer is
visiting a project."
  (interactive)
  (let ((workspace (rustic-buffer-workspace t)))
    (if workspace
        (find-file (concat workspace "/Cargo.toml"))
      (message "The current buffer is not inside a rust project!"))))

;;; _
(provide 'rustic-util)
;;; rustic-util.el ends here
