;;; rustic-babel.el --- Org babel facilities for cargo -*-lexical-binding: t-*-

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)
(require 'ob-core)

(require 'rustic-cargo)
(require 'rustic-compile)

(add-to-list 'org-babel-tangle-lang-exts '("rustic" . "rs"))

(defcustom rustic-babel-display-compilation-buffer nil
  "Whether to display compilation buffer."
  :type 'boolean
  :group 'rustic-babel)

(defcustom rustic-babel-format-src-block t
  "Whether to format a src block automatically after successful execution."
  :type 'boolean
  :group 'rustic-babel)

(defvar rustic-babel-buffer-name '((:default . "*rust-babel*")))

(defvar rustic-babel-process-name "rustic-babel-process"
  "Process name for org-babel rust compilation processes.")

(defvar rustic-babel-compilation-buffer-name "*rustic-babel-compilation-buffer*"
  "Buffer name for org-babel rust compilation process buffers.")

(defvar rustic-babel-dir nil
  "Holds the latest rust babel project directory.")

(defvar rustic-babel-src-location nil
  "Marker, holding location of last evaluated src block.")

(defvar rustic-babel-params nil
  "Babel parameters.")

(defvar rustic-babel-spinner nil)

(defun rustic-babel-eval (dir)
  "Start a rust babel compilation process in directory DIR."
  (let* ((err-buff (get-buffer-create rustic-babel-compilation-buffer-name))
         (default-directory dir)
         (params '("cargo" "build" "--quiet"))
         (inhibit-read-only t))
    (rustic-compilation-setup-buffer err-buff dir 'rustic-compilation-mode)
    (when rustic-babel-display-compilation-buffer
      (display-buffer err-buff))
    (rustic-make-process
     :name rustic-babel-process-name
     :buffer err-buff
     :command params
     :filter #'rustic-compilation-filter
     :sentinel #'rustic-babel-build-sentinel)))

(defun rustic-babel-build-sentinel (proc _output)
  "Sentinel for rust babel compilation process PROC.
If `rustic-babel-format-src-block' is t, format src-block after successful
execution with rustfmt."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (if (zerop (process-exit-status proc))
        (let* ((default-directory rustic-babel-dir))
          ;; format babel block
          (when rustic-babel-format-src-block
            (let ((babel-body
                   (org-element-property :value (org-element-at-point)))
                  (proc
                   (make-process :name "rustic-babel-format"
                                 :buffer "rustic-babel-format-buffer"
                                 :command `(,rustic-rustfmt-bin)
                                 :filter #'rustic-compilation-filter
                                 :sentinel #'rustic-babel-format-sentinel)))
              (while (not (process-live-p proc))
                (sleep-for 0.01))
              (process-send-string proc babel-body)
              (process-send-eof proc)
              (while (eq (process-status proc) 'run)
                (sit-for 0.1))))

          ;; run project
          (let* ((err-buff (get-buffer-create rustic-babel-compilation-buffer-name))
                 (params '("cargo" "run" "--quiet"))
                 (inhibit-read-only t))
            (rustic-make-process
             :name rustic-babel-process-name
             :buffer err-buff
             :command params
             :filter #'rustic-compilation-filter
             :sentinel #'rustic-babel-run-sentinel)))

      (let* ((project (car (reverse (split-string rustic-babel-dir "/"))))
             (result (format "error: Could not compile `%s`." project)))
        (rustic-babel-build-update-result-block result))
      (rustic-with-spinner rustic-babel-spinner nil nil)
      (if (= (length (with-current-buffer proc-buffer (buffer-string))) 0)
          (kill-buffer proc-buffer)
        (pop-to-buffer proc-buffer)))))

(defun rustic-babel-run-sentinel (proc _output)
  "Sentinel for babel project execution."
  (let ((proc-buffer (process-buffer proc))
        result)
    (if (zerop (process-exit-status proc))
        (progn
          (with-current-buffer proc-buffer
            (setq result (buffer-string)))
          (rustic-babel-run-update-result-block result)
          (rustic-with-spinner rustic-babel-spinner nil nil)
          (unless rustic-babel-display-compilation-buffer
            (kill-buffer proc-buffer)))
      (progn
        (with-current-buffer proc-buffer
          (save-excursion
            (save-match-data
              (goto-char (point-min))
              (when (re-search-forward "^thread '[^']+' panicked at '[^']+', ")
                (goto-char (match-beginning 0))
                (setq result (buffer-substring-no-properties (point) (line-end-position)))))))
        (rustic-babel-run-update-result-block result)
        (rustic-with-spinner rustic-babel-spinner nil nil)  
        (pop-to-buffer proc-buffer)))))

(defun rustic-babel-build-update-result-block (result)
  "Update result block with RESULT."
  (let ((marker rustic-babel-src-location)
        (result-params (list (cdr (assq :results rustic-babel-params)))))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (org-babel-remove-result)
      (org-babel-insert-result result))))

(defun rustic-babel-run-update-result-block (result)
  "Update result block with RESULT."
  (let ((marker rustic-babel-src-location))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)

      (let ((file (cdr (assq :file rustic-babel-params)))
            (results-params (cdr (assq :result-params rustic-babel-params))))
    	;; If non-empty result and :file then write to :file.
	    (when (and file results-params)
	      (when result
	        (with-temp-file file
	          (insert (org-babel-format-result
	    	           result (cdr (assq :sep rustic-babel-params))))))
	      (setq result file))

        (org-babel-remove-result rustic-info)
        (org-babel-insert-result result results-params rustic-info)))))

(defun rustic-babel-format-sentinel (proc output)
  "This sentinel is used by the process `rustic-babel-format', that runs
after successful compilation."
  (let ((proc-buffer (process-buffer proc))
        (marker rustic-babel-src-location))
    (save-excursion
      (with-current-buffer proc-buffer
        (when (string-match-p "^finished" output)
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (org-babel-update-block-body
             (with-current-buffer "rustic-babel-format-buffer"
               (buffer-string)))))))
    (kill-buffer "rustic-babel-format-buffer")))

(defun rustic-babel-generate-project (&optional expand)
  "Create rust project in `org-babel-temporary-directory'.
Return full path if EXPAND is t."
  (let* ((default-directory org-babel-temporary-directory)
         (dir (make-temp-file-internal "cargo" 0 "" nil)))
    (shell-command-to-string (format "cargo new %s --bin --quiet" dir))
    (if expand
        (concat (expand-file-name dir) "/")
      dir)))

(defun rustic-babel-project ()
  "In order to reduce the execution time when the project has
dependencies, the project name is stored as a text property in the
header of the org-babel block to check if the project already exists
in `org-babel-temporary-directory'.  If the project exists, reuse it.
Otherwise create it with `rustic-babel-generate-project'."
  (let* ((beg (org-babel-where-is-src-block-head))
         (end (save-excursion (goto-char beg)
                              (line-end-position)))
         (line (buffer-substring beg end)))
    (let* ((project (symbol-name (get-text-property 0 'project line)))
           (path (concat org-babel-temporary-directory "/" project "/")))
      (if (file-directory-p path)
          (progn
            (put-text-property beg end 'project (make-symbol project))
            project)
        (let ((new (rustic-babel-generate-project)))
          (put-text-property beg end 'project (make-symbol new))
          new)))))

(defun rustic-babel-cargo-toml (dir params)
  "Append crates to Cargo.toml.
Use org-babel parameter crates from PARAMS and add them to the project in
directory DIR."
  (let ((crates (cdr (assq :crates params)))
        (toml (expand-file-name "Cargo.toml" dir))
        (dependencies ""))
    (dolist (crate crates)
      (let ((name (symbol-name (car crate)))
            (version (number-to-string (cdr crate))))
        (setq dependencies (concat dependencies name " = " "\"" version "\"" "\n"))))
    (setq dependencies (concat "[dependencies]\n" dependencies) )
    (with-temp-file toml
      (insert-file-contents toml nil)
      (let ((s (nth 0 (split-string (buffer-string) "\\[dependencies]"))))
        (erase-buffer)
        (insert s)
        (insert dependencies)))))

(defun org-babel-execute:rustic (body params)
  "Execute a block of Rust code with org-babel.

If called while there's a live Rust babel process, ask user whether to 
kill the running process."
  (let ((p (get-process rustic-babel-process-name)))
    (if (process-live-p p)
        (let ((buf (process-buffer p)))
          (rustic-process-kill-p p t)
          nil)
      (let* ((default-directory org-babel-temporary-directory)
             (project (rustic-babel-project))
             (dir (setq rustic-babel-dir (expand-file-name project)))
             (main (expand-file-name "main.rs" (concat dir "/src"))))
        (rustic-babel-cargo-toml dir params)
        (setq rustic-info (org-babel-get-src-block-info))
        (setq rustic-babel-params params)

        (rustic-with-spinner rustic-babel-spinner
          (make-spinner rustic-spinner-type t 10)
          '(rustic-babel-spinner (":Executing " (:eval (spinner-print rustic-babel-spinner))))
          (spinner-start rustic-babel-spinner))

        (let ((default-directory dir))
          (write-region
           (concat "#![allow(non_snake_case)]\n" body) nil main nil 0)
          (rustic-babel-eval dir)
          (setq rustic-babel-src-location
                (set-marker (make-marker) (point) (current-buffer)))
          project)))))

(provide 'rustic-babel)
;;; rustic-babel.el ends here
