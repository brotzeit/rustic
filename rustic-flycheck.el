;;; rustic-flycheck.el --- Flycheck support -*-lexical-binding: t-*-

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'let-alist))

(require 'dash)
(require 'flycheck)
(require 'seq)
(require 'json)

(defun rustic-flycheck-find-manifest (file-name)
  "Get the Cargo.toml manifest for FILE-NAME.
FILE-NAME is the path of a file in a cargo project given as a
string.
See http://doc.crates.io/guide.html for an introduction to the
Cargo.toml manifest.
Return the path to the Cargo.toml manifest file, or nil if the
manifest could not be located."
  (-when-let (root-dir (locate-dominating-file file-name "Cargo.toml"))
    (expand-file-name "Cargo.toml" root-dir)))

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
  (let ((cargo (funcall flycheck-executable-find "cargo")))
    (unless cargo
      (user-error "flycheck-rust cannot find `cargo'.  Please \
make sure that cargo is installed and on your PATH.  See \
http://www.flycheck.org/en/latest/user/troubleshooting.html for \
more information on setting your PATH with Emacs."))
    ;; metadata contains a list of packages, and each package has a list of
    ;; targets.  We concatenate all targets, regardless of the package.
    (-when-let (packages (let-alist
                             (with-temp-buffer
                               (call-process cargo nil '(t nil) nil
                                             "metadata" "--no-deps"
                                             "--manifest-path" manifest
                                             "--format-version" "1")
                               (goto-char (point-min))
                               (let ((json-array-type 'list))
                                 (json-read)))
                           .packages))
      (seq-map (lambda (pkg)
                 (let-alist pkg .targets))
               packages))))

(defun rustic-flycheck-find-cargo-target (file-name)
  "Return the Cargo build target associated with FILE-NAME.
FILE-NAME is the path of the file that is matched against the
`src_path' value in the list of `targets' returned by `cargo
read-manifest'.
Return a cons cell (KIND . NAME) where KIND is the target
kind (lib, bin, test, example or bench), and NAME the target
name (usually, the crate name).  If FILE-NAME exactly matches a
target `src-path', this target is returned.  Otherwise, return
the closest matching target, or nil if no targets could be found.
See http://doc.crates.io/manifest.html#the-project-layout for a
description of the conventional Cargo project layout."
  (-when-let* ((manifest (rustic-flycheck-find-manifest file-name))
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
                   (rustic-flycheck-dirs-list file-name
                                              (file-name-directory manifest)))))
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
          (cons (rustic-flycheck-normalize-target-kind .kind) .name))))))


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
                 ((kind . name) (rustic-flycheck-find-cargo-target file-name)))
      (setq-local flycheck-rust-crate-type kind)
      (setq-local flycheck-rust-binary-name name))))

(flycheck-define-checker rustic-clippy
  "A Rust syntax checker using clippy.

     See URL `https://github.com/rust-lang-nursery/rust-clippy'."
  :command ("cargo" "+nightly" "clippy" "--message-format=json")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter flycheck-rust-error-filter
  :error-explainer flycheck-rust-error-explainer

  ;; The reason I had to copy the checker from flycheck
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
