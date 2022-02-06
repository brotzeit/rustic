;; -*- lexical-binding: t -*-

(require 'ert)

(require 'f)
(let ((rustic-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path rustic-dir))

(setq rustic-load-optional-libraries t)
(setq rustic-lsp-setup-p nil) ; don't start LSP server for every test
(setq auto-save-default nil)

(require 'rustic)

(custom-set-variables '(indent-tabs-mode nil))

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

(defsubst rustic-compare-code-after-manip (_original _point-pos _manip-func expected got)
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
         (rustic-format-trigger nil))
    (with-current-buffer buffer
      (write-file file)
      (insert "#[allow(non_snake_case)]")
      (insert string)
      (save-buffer))
    dir))

(defun rustic-test-count-error-helper-new (string)
  (let* ((buffer (get-buffer-create "b"))
         (default-directory org-babel-temporary-directory)
         (dir (rustic-babel-generate-project t))
         (file (expand-file-name "main.rs" (concat dir "/src")))
         (default-directory dir))
    (write-region (concat "#![allow(non_snake_case)]\n" string)
                  nil file nil 0)
    (with-current-buffer buffer
      (find-file file))
    (get-file-buffer file)))

(defmacro rustic-test-silence (messages &rest body)
  `(cl-letf* (((symbol-function 'm)
               (symbol-function #'message))
              ((symbol-function #'message)
	       (lambda (format-string &rest args)
	         (unless (member format-string ,messages)
	           (apply 'm format-string args)))))
     ,@body))

(defun test-indent (indented &optional deindented)
  (let ((deindented
         (or deindented
             (replace-regexp-in-string "^[[:blank:]]*" "      " indented))))
    (rustic-test-manip-code
     deindented
     1
     (lambda ()
       (rustic-test-silence
        '("%s %s"   ; "Indenting..." progress-reporter-do-update
          "%sdone") ; "Indenting...done"  progress-reporter-done
        (indent-region 1 (+ 1 (buffer-size)))))
     indented)))

(defun rustic-test-group-str-by-face (str)
  "Fontify `STR' in rust-mode and group it by face, returning a
list of substrings of `STR' each followed by its face."
  (cl-loop with fontified = (rustic-test-fontify-string str)
           for start = 0 then end
           while start
           for end   = (next-single-property-change start 'face fontified)
           for prop  = (get-text-property start 'face fontified)
           for text  = (substring-no-properties fontified start end)
           if prop
           append (list text prop)))

(defun rustic-test-font-lock (source face-groups)
  "Test that `SOURCE' fontifies to the expected `FACE-GROUPS'"
  (should (equal (rustic-test-group-str-by-face source)
                 face-groups)))

(defun rustic-test-fontify-string (str)
  (with-temp-buffer
    (rustic-mode)
    (insert str)
    (font-lock-ensure)
    (font-lock-flush)
    (buffer-string)))

(defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

(defun rustic-test--wait-till-finished (buffer)
  "Wait till the BUFFER has exited."
  (let* ((proc (get-buffer-process buffer)))
    (while (not (eq (process-status proc) 'exit))
      (sit-for 0.2))))
