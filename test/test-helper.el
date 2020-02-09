;; -*- lexical-binding: t -*-

(require 'ert)
(require 'f)

(let ((rustic-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path rustic-dir))
(require 'rustic)
(custom-set-variables '(indent-tabs-mode nil))

;; don't start LSP server for every test
(setq rustic-lsp-setup-p nil)

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

(defsubst rustic-compare-code-after-manip (original point-pos manip-func expected got)
  (equal expected got))

(defun rustic-test-manip-code (original point-pos manip-func expected)
  (with-temp-buffer
    (rustic-mode)
    (insert original)
    (goto-char point-pos)
    (funcall manip-func)
    (should (rustic-compare-code-after-manip
             original point-pos manip-func expected (buffer-string)))))

;; TODO: rename
(defun rustic-test-count-error-helper (string)
  (let* ((buffer (get-buffer-create "b"))
         (dir (rustic-babel-generate-project t))
         (src (concat dir "/src"))
         (file (expand-file-name "main.rs" src))
         (buffer-save-without-query t)
         (rustic-format-trigger nil))
    (with-current-buffer buffer
      (write-file file)
      (insert "#[allow(non_snake_case)]")
      (insert string)
      (save-buffer))
    dir))
