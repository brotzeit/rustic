;; -*- lexical-binding: t -*-

(require 'ert)
(require 'f)

(let ((rustic-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path rustic-dir))
(require 'rustic)

;; variable doesn't exist in noninteractive emacs sessions
(when noninteractive
  (defvar org-babel-temporary-directory
    (or (and (boundp 'org-babel-temporary-directory)
	         (file-exists-p org-babel-temporary-directory)
	         org-babel-temporary-directory)
	    (make-temp-file "babel-" t))
    "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown.")

  (defun remove-temporary-babel-directory ()
    (when (and (boundp 'org-babel-temporary-directory)
	           (file-exists-p org-babel-temporary-directory))
      (delete-directory org-babel-temporary-directory t)))

  (add-hook 'kill-emacs-hook 'remove-temporary-babel-directory))
