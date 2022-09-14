;;; rustic-babel.el --- org-babel facilities for cargo -*-lexical-binding: t-*-

;;; Code:

(require 'org)
(require 'org-element)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)
(require 'ob-core)

(require 'rustic-rustfmt)

;; FIXME This variable doesn't exist in noninteractive emacs sessions,
;; which probably means that it is internal and we shouldn't use it.
(defvar org-babel-temporary-directory)

(defvar rustic-info nil)

(add-to-list 'org-src-lang-modes '("rust" . rustic))
(add-to-list 'org-babel-tangle-lang-exts '("rustic" . "rs"))
(defalias 'org-babel-execute:rust #'org-babel-execute:rustic)

(defcustom rustic-babel-display-compilation-buffer nil
  "Whether to display compilation buffer."
  :type 'boolean
  :group 'rustic-babel)

(defcustom rustic-babel-auto-wrap-main t
  "Whether to auto wrap body in 'fn main' to function call if none exists."
  :type 'boolean
  :group 'rustic-babel)

(defcustom rustic-babel-format-src-block t
  "Whether to format a src block automatically after successful execution."
  :type 'boolean
  :group 'rustic-babel)

(defcustom rustic-babel-default-toolchain "stable"
  "Active toolchain for babel blocks.
When passing a toolchain to a block as argument, this variable won't be
considered."
  :type 'string
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

(defun rustic-babel-eval (dir toolchain-kw-or-string)
  "Start a rust babel compilation process in directory DIR."
  (let* ((err-buff (get-buffer-create rustic-babel-compilation-buffer-name))
         (default-directory dir)
         (toolchain (cond ((eq toolchain-kw-or-string 'nightly) "+nightly")
                          ((eq toolchain-kw-or-string 'beta) "+beta")
                          ((eq toolchain-kw-or-string 'stable) "+stable")
                          (toolchain-kw-or-string (format "+%s" toolchain-kw-or-string))
                          (t (format "+%s" rustic-babel-default-toolchain))))
         (params (list "cargo" toolchain "build" "--quiet"))
         (inhibit-read-only t))
    (rustic-compilation-setup-buffer err-buff dir 'rustic-compilation-mode)
    (when rustic-babel-display-compilation-buffer
      (display-buffer err-buff))
    (rustic-make-process
     :name rustic-babel-process-name
     :buffer err-buff
     :command params
     :filter #'rustic-compilation-filter
     :sentinel (lambda (proc output)
                 (rustic-babel-build-sentinel toolchain proc output)))))

(defun rustic-babel-format-block ()
  "Format babel block at point."
  (interactive)
  (save-excursion
    (let ((babel-body
           (org-element-property :value (org-element-at-point)))
          (proc
           (make-process :name "rustic-babel-format"
                         :buffer "rustic-babel-format-buffer"
                         :command `(,(rustic-rustfmt-bin)
                                    ,@(rustic-compute-rustfmt-args))
                         :filter #'rustic-compilation-filter
                         :sentinel #'rustic-babel-format-sentinel)))
      (while (not (process-live-p proc))
        (sleep-for 0.01))
      (process-send-string proc babel-body)
      (process-send-eof proc)
      proc)))

(defun rustic-babel-build-sentinel (toolchain proc _output)
  "Sentinel for rust babel compilation process PROC.
If `rustic-babel-format-src-block' is t, format src-block after successful
execution with rustfmt."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (if (zerop (process-exit-status proc))
        (let* ((default-directory rustic-babel-dir))

          ;; format babel block
          (when rustic-babel-format-src-block
            (let ((proc (rustic-babel-format-block)))
              (while (eq (process-status proc) 'run)
                (sit-for 0.1))))

          ;; run project
          (let* ((err-buff (get-buffer-create rustic-babel-compilation-buffer-name))
                 (params (list "cargo" toolchain "run" "--quiet"))
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
  (let ((marker rustic-babel-src-location))
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
  (ignore-errors
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
      (kill-buffer "rustic-babel-format-buffer"))))

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

(defun crate-dependencies (name version features path)
  "Generate a Cargo.toml [dependencies] entry for a crate given a version and features."
  (let* ((version-string (concat "version = \"" version "\""))
         (features-string
          (when features
            (concat "features = [" (mapconcat (lambda (s) (concat "\"" s "\"")) features ", ") "]")))
         (path-string
          (when path
            (concat "path = \"" path "\"")))
         (toml-entry (string-join (remove nil (list version-string features-string path-string)) ", ")))
    (concat name " = {" toml-entry "}")))

(defun cargo-toml-dependencies (crate-versions crate-features crate-paths)
  "Generate the [dependencies] section of a Cargo.toml file given crates and their versions & features."
  (let ((dependencies ""))
    (dolist (crate-and-version crate-versions)
      (let* ((name (if (listp crate-and-version)
                       (car crate-and-version)
                     crate-and-version))
             (version (if (listp crate-and-version)
                          (cdr crate-and-version)
                        "*"))
             (features (cdr (assoc name crate-features)))
             (path (cdr (assoc name crate-paths))))

        ;; make sure it works with symbols and strings
        (when (symbolp name)
          (setq name (symbol-name name)))
        (when (numberp version)
          (setq version (number-to-string version)))
        (when (symbolp version)
          (setq version (symbol-name version)))

        (when (not (listp features))
          (setq features (list features)))
        (let ((cargo-toml-entry (crate-dependencies name version features path)))
          (setq dependencies (concat dependencies cargo-toml-entry "\n")))))
    (setq dependencies (concat "[dependencies]\n" dependencies))))

(defun rustic-babel-cargo-toml (dir params)
  "Append crates to Cargo.toml.
Use org-babel parameter crates from PARAMS and add them to the project in
directory DIR."
  (let ((crates (cdr (assq :crates params)))
        (features (cdr (assq :features params)))
        (paths (cdr (assq :paths params)))
        (toml (expand-file-name "Cargo.toml" dir)))
    (let ((dependencies (cargo-toml-dependencies crates features paths)))
      (make-directory (file-name-directory toml) t)
      (with-temp-file toml
        (condition-case nil
            (insert-file-contents toml)
          (file-missing))
        (let ((s (nth 0 (split-string (buffer-string) "\\[dependencies]"))))
          (erase-buffer)
          (insert s)
          (insert dependencies))))))

(defun rustic-babel-ensure-main-wrap (body)
  "Wrap BODY in a 'fn main' function call if none exists."
  (if (string-match "^[ \t]*\\(pub \\)?\\(async \\)?[fn]+[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "fn main() {\n%s\n}\n" body)))

(defun rustic-babel-include-blocks (blocks)
  "Insert contents of BLOCKS to the 'main block' that is being
executed with the parameter `:include'."
  (let ((contents ""))
    (with-current-buffer (current-buffer)
      (save-excursion
        (dolist (b (mapcar (lambda (b) (if (symbolp b) (symbol-name b) b)) blocks))
          (when-let ((c (rustic-babel-block-contents b)))
            (setq contents (concat contents c))))))
    contents))

(defun rustic-babel-block-contents (block-name)
  "Return contents of block with the name BLOCK-NAME"
  (with-current-buffer (current-buffer)
    (save-excursion
      (org-babel-goto-named-src-block block-name)
      (org-element-property :value (org-element-at-point)))))

(defun rustic-babel-insert-mod (mods)
  "Build string with module declarations for MODS and return it."
  (let ((c ""))
    (dolist (m mods)
      (setq c (concat c (format "mod %s;\n" m))))
    c))

(defun rustic-babel-generate-modules (root blocks)
  "Create module files for BLOCKS in the project with ROOT."
  (dolist (b (mapcar (lambda (b) (if (symbolp b) (symbol-name b) b)) blocks))
    (let* ((contents (rustic-babel-block-contents b))
           (src-dir (concat root "/src/"))
           (module (expand-file-name (format "%s.rs" b) src-dir)))
      (write-region contents nil module nil 0))))

(defun org-babel-execute:rustic (body params)
  "Execute a block of Rust code with org-babel.

If called while there's a live Rust babel process, ask user whether to
kill the running process."
  (let ((p (get-process rustic-babel-process-name)))
    (if (process-live-p p)
        (progn
          (rustic-process-kill-p p t)
          nil)
      (let* ((default-directory org-babel-temporary-directory)
             (project (rustic-babel-project))
             (dir (setq rustic-babel-dir (expand-file-name project)))
             (main-p (cdr (assq :main params)))
             (main (expand-file-name "main.rs" (concat dir "/src")))
             (wrap-main (cond ((string= main-p "yes") t)
                              ((string= main-p "no") nil)
                              (t rustic-babel-auto-wrap-main)))
             (include-blocks (cdr (assq :include params)))
             (use-blocks (cdr (assq :use params))))
        (make-directory (file-name-directory main) t)
        (rustic-babel-cargo-toml dir params)
        (when use-blocks
          (rustic-babel-generate-modules dir use-blocks))
        (setq rustic-info (org-babel-get-src-block-info))
        (setq rustic-babel-params params)

        (rustic-with-spinner rustic-babel-spinner
          (make-spinner rustic-spinner-type t 10)
          '(rustic-babel-spinner (":Executing " (:eval (spinner-print rustic-babel-spinner))))
          (spinner-start rustic-babel-spinner))

        (let ((default-directory dir)
              (toolchain (cdr (assq :toolchain params))))
          (write-region
           (concat "#![allow(non_snake_case, unused)]\n"
                   (if use-blocks (rustic-babel-insert-mod use-blocks) "")
                   (if include-blocks (rustic-babel-include-blocks include-blocks) "")
                   (if wrap-main (rustic-babel-ensure-main-wrap body) body))
           nil main nil 0)
          (rustic-babel-eval dir toolchain)
          (setq rustic-babel-src-location
                (set-marker (make-marker) (point) (current-buffer)))
          project)))))

(defun rustic-babel-visit-project ()
  "Open main.rs of babel block at point. The block has to be executed
at least one time in this emacs session before this command can be used."
  (interactive)
  (let* ((beg (org-babel-where-is-src-block-head))
         (end (save-excursion (goto-char beg)
                              (line-end-position)))
         (line (buffer-substring beg end))
         (project (symbol-name (get-text-property 0 'project line)))
         (path (concat org-babel-temporary-directory "/" project "/src/main.rs")))
    (if (file-exists-p path)
        (find-file path)
      (message "Run block first to visit generated project."))))

;; TODO: honor babel params
(defun rustic-babel-clippy ()
  "Currently doesn't support params."
  (interactive)
  (let* ((err-buff (get-buffer-create rustic-babel-compilation-buffer-name))
         (default-directory org-babel-temporary-directory)
         (body (org-element-property :value (org-element-at-point)))
         (project (rustic-babel-project))
         (params (list "cargo" "clippy")))
    (let* ((dir (setq rustic-babel-dir (expand-file-name project)))
           (main (expand-file-name "main.rs" (concat dir "/src")))
           (default-directory dir))
      (write-region
       (concat "#![allow(non_snake_case)]\n" body)
       nil main nil 0)
      (rustic-compilation-setup-buffer err-buff dir 'rustic-cargo-clippy-mode)
      (display-buffer err-buff)
      (rustic-make-process
       :name rustic-babel-process-name
       :buffer err-buff
       :command params
       :filter #'rustic-compilation-filter))))

(defun rustic-babel-lookup-missing-dependencies ()
  (let* ((contents (org-element-property :value (org-element-at-point)))
         crates)
    (dolist (line (split-string contents "\n"))
      (when (string-match "\s+use\s" line)
        (if (string-match "::" line)
            (let* ((import (nth 1 (split-string line)))
                   (c (nth 0 (split-string import "::"))))
              (unless (string-match "^std" c)
                (push c crates)))
          (let* ((import (nth 1 (split-string line))))
            (unless (string-match "^std" import)
              (push (string-trim-right import ";") crates))))))
    crates))

(defun rustic-babel-header-insert-crates ()
  "Parse crates from imports and insert them with the header arg `:crates'.
Ignore crates that are listed in `:use'."
  (interactive)
  (let* ((crates (rustic-babel-lookup-missing-dependencies))
         (use-blocks (cdr (assq :use (nth 2 (org-babel-get-src-block-info)))))
         (missing (--filter
                   (not (-contains?
                         (mapcar (lambda (b) (if (symbolp b) (symbol-name b) b)) use-blocks) it))
                   crates)))
    (org-babel-insert-header-arg (concat "crates '" (format "%s" missing)))))

(provide 'rustic-babel)
;;; rustic-babel.el ends here
