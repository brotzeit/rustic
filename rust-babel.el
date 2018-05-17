;;; rust-babel.el --- Org babel facilities for rust-mode -*-lexical-binding: t-*-

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)
(require 'ob-core)

(add-to-list 'org-babel-tangle-lang-exts '("rust" . "rs"))

(defcustom rust-babel-display-compilation-buffer nil
  "Whether to display compilation buffer."
  :type 'boolean
  :group 'rust-mode)

(defvar rust-babel-buffer-name '((:default . "*rust-babel*")))

(defvar rust-babel-process-name "rust-babel-process"
  "Process name for org-babel rust compilation processes.")

(defvar rust-babel-compilation-buffer "*rust-babel-compilation-buffer*"
  "Buffer name for org-babel rust compilation process buffers.")

(defvar rust-babel-dir nil
  "Holds the latest rust babel project directory.")

(defvar rust-babel-params nil)

(defun rust-org-babel-eval (dir)
  "Start a rust babel compilation process."
  (let* ((err-buff (get-buffer-create rust-babel-compilation-buffer))
         (default-directory dir)
         (coding-system-for-read 'binary)
         (process-environment (nconc
	                           (list (format "TERM=%s" "ansi"))
                               process-environment))
         (params '("cargo" "build"))
         (inhibit-read-only t))
    (with-current-buffer err-buff
      (erase-buffer)
      (setq-local default-directory dir)
      (rust-compilation-mode))
    (if rust-babel-display-compilation-buffer
     (display-buffer err-buff))
    (let ((proc (make-process
                 :name rust-babel-process-name
                 :buffer err-buff
                 :command params
                 :filter #'rust-compile-filter
                 :sentinel #'rust-babel-sentinel))))))

(defun rust-babel-sentinel (proc string)
  "Sentinel for rust babel compilation processes."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (if (zerop (process-exit-status proc))
        (let* ((default-directory rust-babel-dir) 
               (result (shell-command-to-string "cargo run --quiet"))
               (result-params (list (cdr (assq :results rust-babel-params))))
               (params rust-babel-params)
               (marker rust-babel-src-location))
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (org-babel-remove-result rust-info)
            (org-babel-insert-result result result-params rust-info))
          (unless rust-babel-display-compilation-buffer
           (kill-buffer proc-buffer)))
      (pop-to-buffer proc-buffer))))

(defun rust-babel-generate-project ()
  "Create rust project in `org-babel-temporary-directory'."
  (let* ((default-directory org-babel-temporary-directory)
         (dir (make-temp-file-internal "cargo" 0 "" nil)))
    (shell-command-to-string (format "cargo new %s --bin --quiet" dir))
    (setq rust-babel-dir (expand-file-name dir))))

(defun rust-babel-cargo-toml (dir params)
  "Append crates to Cargo.toml."
  (let ((crates (cdr (assq :crates params)))
        (toml (expand-file-name "Cargo.toml" dir))
        (str ""))
    (dolist (crate crates)
      (setq str (concat str (car crate) " = " "\"" (cdr crate) "\"" "\n")))
    (write-region str nil toml t)))

(defun org-babel-execute:rust (body params)
  "Execute a block of Rust code with Babel."
  (let* ((full-body (org-element-property :value (org-element-at-point)))
         (dir (rust-babel-generate-project))
         (project (car (reverse (split-string rust-babel-dir "\\/"))))
         (main (expand-file-name "main.rs" (concat dir "/src"))))
    (setq rust-info (org-babel-get-src-block-info))
    (rust-babel-cargo-toml dir params)
    (setq rust-babel-params params)
    (let ((default-directory dir))
      (write-region full-body nil main nil 0)
      (rust-org-babel-eval dir)
      (setq rust-babel-src-location (set-marker (make-marker) (point) (current-buffer)))
      project)))

(provide 'rust-babel)
;;; rust-babel.el ends here
