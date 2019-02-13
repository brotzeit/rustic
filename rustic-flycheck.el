;;; rustic-flycheck.el --- Flycheck support -*-lexical-binding: t-*-

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'let-alist)
  (require 'subr-x))

(require 'dash)
(require 'flycheck)
(require 'seq)
(require 'json)

(defcustom rustic-flycheck-setup-mode-line-p t
  "Whether to display errors in the mode-line."
  :type 'boolean
  :group 'rustic-flycheck)

(defun rustic-flycheck-lighter (state)
  "Return flycheck information for the given error type STATE.

Source: https://git.io/vQKzv"
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))

(when rustic-flycheck-setup-mode-line-p
 (add-to-list 'mode-line-misc-info
              '(:eval
                (when (and (bound-and-true-p flycheck-mode)
                           (or flycheck-current-errors
                               (eq 'running flycheck-last-status-change)))
                  (concat
                   (cl-loop for state in '((error . "#FB4933")
                                           (warning . "#FABD2F")
                                           ;; (info . "#83A598")
                                           )
                            as lighter = (rustic-flycheck-lighter (car state))
                            when lighter
                            concat (propertize
                                    lighter
                                    'face `(:foreground ,(cdr state))))
                   " ")))))

(defun rustic-flycheck-dirs-list (start end)
  "Return a list of directories from START (inclusive) to END (exclusive).
E.g., if START is '/a/b/c/d' and END is '/a', return the list
'(/a/b/c/d /a/b/c /a/b) in this order.
START and END are strings representing file paths.  END should be
above START in the file hierarchy; if not, the list stops at the
root of the file hierarchy."
  (let ((dirlist)
        (dir (expand-file-name start))
        (end (expand-file-name end)))
    (while (not (or (equal dir (car dirlist)) ; avoid infinite loop
                    (file-equal-p dir end)))
      (push dir dirlist)
      (setq dir (directory-file-name (file-name-directory dir))))
    (nreverse dirlist)))

(defun rustic-flycheck-get-cargo-targets (manifest)
  "Return the list of available Cargo targets for the given project.
MANIFEST is the path to the Cargo.toml file of the project.
Calls `cargo metadata --no-deps --manifest-path MANIFEST
--format-version 1', parses and collects the targets for the
current workspace, and returns them in a list, or nil if no
targets could be found."
  (let ((process-output-as-json
         (lambda (program &rest args)
           (with-temp-buffer
             (let ((code-or-signal (apply 'call-process program nil '(t nil) nil args)))
               (unless (equal code-or-signal 0)
                 ;; Prevent from displaying "JSON readtable error".
                 (let* ((args (combine-and-quote-strings (cons program args)))
                        (error-message (if (stringp code-or-signal)
                                           (format "%s terminated by %s." args code-or-signal)
                                         (format "%s exited with %s." args code-or-signal))))
                   (user-error error-message)))
               (goto-char (point-min))
               (let ((json-array-type 'list))
                 (json-read))))))
        (cargo (funcall flycheck-executable-find "cargo")))
    (unless cargo
      (user-error "flycheck-rust cannot find `cargo'.  Please \
make sure that cargo is installed and on your PATH.  See \
http://www.flycheck.org/en/latest/user/troubleshooting.html for \
more information on setting your PATH with Emacs."))
    ;; metadata contains a list of packages, and each package has a list of
    ;; targets.  We concatenate all targets, regardless of the package.
    (-when-let (packages (let-alist
                             (funcall process-output-as-json
                                      cargo "metadata"
                                      "--no-deps"
                                      "--manifest-path" manifest
                                      "--format-version" "1")
                           .packages))
      (seq-map (lambda (pkg)
                 (let-alist pkg .targets))
               packages))))

(defun rustic-flycheck-find-cargo-target (file-name)
  "Return the Cargo build target associated with FILE-NAME.

FILE-NAME is the path of the file that is matched against the
`src_path' value in the list of `targets' returned by `cargo
read-manifest'.

Return an alist ((KIND . k) (NAME . n) (REQUIRED-FEATURES . rf))
where KIND is the target kind (lib, bin, test, example or bench),
NAME the target name (usually, the crate name), and REQUIRED-FEATURES is the
optional list of features required to build the selected target.  If FILE-NAME
exactly matches a target `src-path', this target is returned.  Otherwise, return
the closest matching target, or nil if no targets could be found.

See http://doc.crates.io/manifest.html#the-project-layout for a
description of the conventional Cargo project layout."
  (-when-let* ((workspace (rustic-buffer-workspace t))
               (manifest (concat workspace "Cargo.toml"))
               (packages (rustic-flycheck-get-cargo-targets manifest))
               (targets (-flatten-n 1 packages)))
    (let ((target
           (or
            ;; If there is a target that matches the file-name exactly, pick
            ;; that one
            (seq-find (lambda (target)
                        (let-alist target (string= file-name .src_path)))
                      targets)
            ;; Otherwise find the closest matching target by walking up the tree
            ;; from FILE-NAME and looking for targets in each directory.  E.g.,
            ;; the file 'tests/common/a.rs' will look for a target in
            ;; 'tests/common', then in 'tests/', etc.
            (car (seq-find
                  (lambda (pair)
                    (-let [((&alist 'src_path target-path) . dir) pair]
                      (file-equal-p dir (file-name-directory target-path))))
                  ;; build a list of (target . dir) candidates
                  (-table-flat
                   'cons targets
                   (rustic-flycheck-dirs-list file-name workspace))))
            ;; If all else fails, just pick the first target
            (car targets))))
      ;; If target is 'custom-build', we pick another target from the same package (see GH-62)
      (when (string= "custom-build" (let-alist target (car .kind)))
        (setq target (->> packages
                          ;; find the same package as current build-script buffer
                          (--find (--any? (equal target it) it))
                          (--find (not (equal target it))))))
      (when target
        (let-alist target
          (seq-filter (lambda (kv) (cdr kv))
                     (list (cons 'kind (rustic-flycheck-normalize-target-kind .kind))
                           (cons 'name .name)
                           (cons 'required-features .required-features))))))))

(defun rustic-flycheck-normalize-target-kind (kinds)
  "Return the normalized target name from KIND.
KIND is a list of target name as returned by `cargo metadata',
which do not necessarily correspond to to target names that can
be passed as argument to `cargo rustc'.
The normalization returns a valid cargo target based on KINDS."
  ;; Assumption: we only care about the first kind name.  Multiple kinds only
  ;; seem to happen for library crate types, and those all maps to the same
  ;; `lib' target.
  (pcase (car kinds)
    (`"dylib" "lib")
    (`"rlib" "lib")
    (`"staticlib" "lib")
    (`"cdylib" "lib")
    (`"proc-macro" "lib")
    (_ (car kinds))))

;;;###autoload
(defun rustic-flycheck-setup ()
  "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout."
  (interactive)
  ;; We should avoid raising any error in this function, as in combination
  ;; with `global-flycheck-mode' it will render Emacs unusable (see
  ;; https://github.com/flycheck/flycheck-rust/issues/40#issuecomment-253760883).
  (with-demoted-errors "Error in rustic-flycheck-setup: %S"
    (-when-let* ((file-name (buffer-file-name))
                 (target (rustic-flycheck-find-cargo-target file-name)))
      (let-alist target
        (setq-local flycheck-rust-features .required-features)
        (setq-local flycheck-rust-crate-type .kind)
        (setq-local flycheck-rust-binary-name .name)))))

(flycheck-define-checker rustic-clippy
  "A Rust syntax checker using clippy.

  See URL `https://github.com/rust-lang-nursery/rust-clippy'."
  :command ("cargo" "clippy" "--message-format=json")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter flycheck-rust-error-filter
  :error-explainer flycheck-rust-error-explainer
  :modes rustic-mode
  :predicate flycheck-buffer-saved-p
  :enabled (lambda ()
             (and (flycheck-rust-cargo-has-command-p "clippy")
                  (flycheck-rust-manifest-directory)))
  :working-directory (lambda (_) (flycheck-rust-manifest-directory))
  :verify
  (lambda (_)
    (and buffer-file-name
         (let ((has-toml (flycheck-rust-manifest-directory))
               (has-clippy (flycheck-rust-cargo-has-command-p "clippy")))
           (list
            (flycheck-verification-result-new
             :label "Clippy"
             :message (if has-clippy "Found"
                        "Cannot find the `cargo clippy' command")
             :face (if has-clippy 'success '(bold warning)))
            (flycheck-verification-result-new
             :label "Cargo.toml"
             :message (if has-toml "Found" "Missing")
             :face (if has-toml 'success '(bold warning))))))))

(push 'rustic-clippy flycheck-checkers)
(add-hook 'rustic-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook #'rustic-flycheck-setup)

(provide 'rustic-flycheck)
;;; rustic-flycheck.el ends here
