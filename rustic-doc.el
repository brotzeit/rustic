;;; rustic-doc.el --- Browse rust documentation as .org files -*- lexical-binding: t -*-

;; Copyright (c) 2020 Sam Hedin

;; Author: Sam Hedin <sam.hedin@gmail.com>
;;         Jonas Møller <jonas.moeller2@protonmail.com>

;;; Commentary:

;; This package lets you convert rustic-doc html-files to org mode
;; files, and lets you browse them with `rustic-doc-search'.

;; Run `M-x rustic-doc-setup' to download the required files and
;;   convert the rust standard library.

;; Run `M-x rustic-doc-convert-current-package' to generate and
;; convert docs for the package you are currently visiting.

;;; Code:

(require 'url)
(require 'f)

(eval-and-compile
  (if (< emacs-major-version 27)
      (defun rustic-doc--xdg-data-home ()
        (or (getenv "XDG_DATA_HOME")
            (concat (file-name-as-directory (getenv "HOME"))
                    ".local/share")))
    (require 'xdg)
    (fset 'rustic-doc--xdg-data-home 'xdg-data-home)))

(defvar rustic-doc-lua-filter (concat (file-name-as-directory (getenv "HOME"))
                                      ".local/bin/rustic-doc-filter.lua")
  "Save location for the rustic-doc lua filter.")

(defvar rustic-doc-convert-prog (concat (file-name-as-directory (getenv "HOME"))
                                        ".local/bin/rustic-doc-convert.sh")
  "Save location for the rustic-doc conversion script.")

(defvar rustic-doc-source-repo
  "https://raw.githubusercontent.com/emacs-rustic/rustic/main/rustic-doc/")

(defvar rustic-doc-current-project nil
  "Location to search for documentation.
All projects and std by default, otherwise last open project and std.")

(defvar rustic-doc-save-loc (concat (rustic-doc--xdg-data-home)
                                    "/emacs/rustic-doc"))

(defvar rustic-doc-resources
  `((,rustic-doc-convert-prog
     (:exec)
     ,(concat rustic-doc-source-repo "convert.sh"))
    (,rustic-doc-lua-filter
     ()
     ,(concat rustic-doc-source-repo "filter.lua"))))

(defun rustic-doc-default-rg-search-command ()
  "The default search command when using helm-ag.
Needs to be a function because of its reliance on
`rustic-doc-current-project'"
  (concat "rg --smart-case --no-heading --color=never --line-number "
          (if rustic-doc-current-project " -L" "")))

(defcustom rustic-doc-rg-search-command 'rustic-doc-default-rg-search-command
  "The default command string to pass helm-ag when searching."
  :type 'function
  :group 'rustic-doc)

(defvar helm-ag-base-command)
(defvar helm-ag-success-exit-status)
(declare-function helm-ag "ext:helm-ag")

(defun rustic-doc-default-search-function (search-dir search-term)
  "Default search functionality.
Uses helm-ag and ripgrep if possible, grep otherwise.
Search for SEARCH-TERM inside SEARCH-DIR"
  (cond
   ((and  (require 'helm-ag nil t) (executable-find "rg"))
    (let* ((helm-ag-base-command (funcall rustic-doc-rg-search-command))
           (helm-ag-success-exit-status '(0 2)))
      (condition-case nil
          (helm-ag search-dir search-term)
        ;; If the search didn't turn anything up we re-run the search
        ;; in the top level searchdir.
        (error (helm-ag rustic-doc-save-loc search-term)))))
   ((executable-find "rg")
    (grep (format "%s '%s' %s"
                  (rustic-doc-default-rg-search-command)
                  search-term
                  search-dir)))
   (t
    (grep (format "grep -RPIni '%s' %s" search-term search-dir)))))


(defcustom rustic-doc-search-function 'rustic-doc-default-search-function
  "Function to use for searching documentation.
The function should take search-dir and search-term as arguments."
  :type 'function
  :group 'rustic-doc)



(defun rustic-doc--install-resources ()
  "Install or update the rustic-doc resources."
  (dolist (resource rustic-doc-resources)
    (pcase resource
      (`(,dst ,opts ,src)
       (condition-case nil
           (progn
             (unless (f-exists? (f-dirname dst))
               (f-mkdir (f-dirname dst)))
             (f-exists? (f-dirname dst))
             (url-copy-file src dst t)
             (when (memq :exec opts)
               (call-process (executable-find "chmod")
                             nil
                             nil
                             nil
                             "+x"
                             dst)))
         (error (progn
                  (if (file-exists-p dst)
                      (message (format "Could not update %s, using existing one"
                                       dst))
                    (error (format "Could not retrieve %s" dst)))))))
      (x (error "Invalid resource spec: %s" x)))))

;;;###autoload
(defun rustic-doc-dumb-search (search-term)
  "Search all projects and std for SEARCH-TERM.
Use this when `rustic-doc-search' does not find what you're looking for.
Add `universal-argument' to only search level 1 headers.
See `rustic-doc-search' for more information."
  (interactive (let ((short-name (alist-get 'short-name
                                            (rustic-doc--thing-at-point))))
                 (list (read-string (format "search term, default %s: " short-name)
                                    nil
                                    nil
                                    short-name))))
  (rustic-doc-search search-term t))


;;;###autoload
(defun rustic-doc-search (search-term &optional root)
  "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results.
To limit search results to only level 1 headers, add `universal-argument'
Level 1 headers are things like struct or enum names.
if ROOT is non-nil the search is performed from the root dir.
This function tries to be smart and limits the search results
as much as possible. If it ends up being so smart that
it doesn't manage to find what you're looking for, try `rustic-doc-dumb-search'."
  (interactive (let ((short-name (alist-get 'short-name
                                            (rustic-doc--thing-at-point))))
                 (list (read-string (format "search term, default %s: " short-name)
                                    nil
                                    nil
                                    short-name))))

  (rustic-doc--update-current-project)
  (let* ((thing-at-point (rustic-doc--thing-at-point))
         (short-name (alist-get 'short-name thing-at-point))
         ;; If the user did not accept the default search suggestion,
         ;; we should not search in that suggestion's directory.
         (search-dir
          (cond
           (root rustic-doc-save-loc)
           ((string-equal short-name search-term)
            (alist-get 'search-dir thing-at-point))
           (t (rustic-doc--project-doc-dest))))
         ;; If the prefix arg is provided, we only search for level 1
         ;; headers by making sure that there is only one * at the
         ;; beginning of the line.
         (regex (if current-prefix-arg
                    (progn
                      (setq current-prefix-arg nil)
                      "^\\*")
                  "^\\*+"))
         ;; This seq-reduce turns `enum option' into (kind of)
         ;; `enum.*option', which lets there be chars between the
         ;; searched words
         (regexed-search-term
          (concat regex
                  ;; Regex explanation
                  ;; `-' => Do not match if a return type.  A search
                  ;;        for Option should not show is_some -> Option
                  ;; `(' => Do not match if it's an argument name.
                  ;; `<' => Do not match if it's a generic type arg
                  (seq-reduce (lambda (acc s)
                                (concat acc "[^-\*(<]*" s))
                              (split-string search-term " ")
                              ""))))
    (unless (file-directory-p rustic-doc-save-loc)
      (rustic-doc-setup)
      (message "Running first time setup. Please re-run your search\
 once conversion has completed."))
    ;; If the user has not run `rustic-doc-convert-current-package' in
    ;; the current project, we create a default directory that only
    ;; contains a symlink to std.
    (unless (file-directory-p (rustic-doc--project-doc-dest))
      (rustic-doc-create-project-dir))
    (funcall rustic-doc-search-function search-dir regexed-search-term)))

(defun rustic-doc--update-current-project ()
  "Update `rustic-doc-current-project' if editing a rust file, otherwise leave it."
  (when (and (featurep  'lsp-mode)
             (derived-mode-p 'rust-mode 'rustic-mode))
    (setq rustic-doc-current-project (lsp-workspace-root))))

(defun rustic-doc--deepest-dir (path)
  "Find the deepest existing and non-empty arg-directory parent of PATH.
We can sometimes infer the filepath from the crate name.
E.g the enum std::option::Option is in the folder std/option.
Some filepaths can not be inferred properly, seemingly because of
URL `https://github.com/rust-lang/rust/issues/21934'.
In these cases, the deepest dir will be the current project dir."
  (if (and (file-exists-p path)
           (file-directory-p path)
           (not (f-empty-p path)))
      path
    (rustic-doc--deepest-dir (f-slash (f-dirname path)))))

(defun rustic-doc--project-doc-dest ()
  "The location of the documentation for the current or last seen project.
If the user has not visited a project, returns the main doc directory."
  (if rustic-doc-current-project
      (f-join rustic-doc-save-loc
              (f-filename rustic-doc-current-project))
    rustic-doc-save-loc))

(defun rustic-doc-create-project-dir ()
  "Create a rustic-doc arg-directory for the current project. Link with std."
  (let* ((link-tgt (concat (file-name-as-directory (rustic-doc--xdg-data-home))
                           "emacs/rustic-doc/std"))
         (link-name (concat (rustic-doc--project-doc-dest)
                            "/std"))
         (current-doc-dest (rustic-doc--project-doc-dest)))
    (if current-doc-dest
        (progn
          (make-directory (rustic-doc--project-doc-dest)
                          t)
          (make-symbolic-link link-tgt link-name t))
      (message "Couldn't create project doc directory."))))

;;;###autoload
(defun rustic-doc-convert-current-package ()
  "Convert the documentation for a project and its dependencies."
  (interactive)
  (unless (file-directory-p rustic-doc-save-loc)
    (rustic-doc-setup)
    (message "Running first time setup."))
  (if rustic-doc-current-project
      (progn
        (message "Converting documentation for %s "
                 rustic-doc-current-project)
        (if (/= 0 (call-process "cargo" nil "*cargo-makedocs*" nil "makedocs"))
            (message "\
cargo makedocs could not generate docs for the current package. \
See buffer *cargo-makedocs* for more info")
          (let* ((docs-src
                  (concat (file-name-as-directory rustic-doc-current-project)
                          "target/doc"))
                 (finish-func (lambda (_p)
                                (message "Finished converting docs for %s"
                                         rustic-doc-current-project))))
            (rustic-doc-create-project-dir)
            (rustic-doc--start-process "rustic-doc-convert"
                                       rustic-doc-convert-prog
                                       finish-func
                                       docs-src
                                       (rustic-doc--project-doc-dest)))))
    (message "Activate rustic-doc-mode to run `rustic-doc-convert-current-package")))

(defun rustic-doc--confirm-dep-versions (missing-fd)
  "Verify that dependencies are not too old.
Do not check `fd' when MISSING-FD is non-nil."
  (when (not missing-fd)
    (when  (> 8 (string-to-number
                  (substring (shell-command-to-string "fd --version") 3 4)))
      (message "Your version of fd is too old, please install a recent version, maybe through cargo.")))

  (when (>= 11 (string-to-number
                (substring (shell-command-to-string "pandoc --version") 9 11)))
    (message "Your version of pandoc is too old, please install a more recent version. See their github for more info.")))


(defun rustic-doc-install-deps (&optional noconfirm)
  "Install dependencies with Cargo.
If NOCONFIRM is non-nil, install all dependencies without prompting user."
  (if (not (executable-find "cargo"))
      (message "You need to have cargo installed to use rustic-doc")
    (let ((missing-rg (not (executable-find "rg")))
          (missing-fd (and  (not (executable-find "fd") )))
          (missing-makedocs (not (executable-find "cargo-makedocs"))))
      (rustic-doc--confirm-dep-versions missing-fd)
      (when (and (or missing-fd missing-makedocs missing-rg)
                 (or noconfirm (y-or-n-p "Missing some dependencies for rustic doc, install them? ")))
        (when missing-fd
          (rustic-doc--start-process "install-fd" (rustic-cargo-bin) nil "install" "fd-find"))
        (when missing-rg
          (rustic-doc--start-process "install-rg" (rustic-cargo-bin)  nil "install" "ripgrep"))
        (when missing-makedocs
          (rustic-doc--start-process "install-makedocs" (rustic-cargo-bin) nil
                                     "install" "cargo-makedocs"))))))

;;;###autoload
(defun rustic-doc-setup (&optional no-dl noconfirm)
  "Setup or update rustic-doc filter and convert script. Convert std.
If NO-DL is non-nil, will not try to re-download
the pandoc filter and bash script.
NO-DL is primarily used for development of the filters.
If NOCONFIRM is non-nil, install all dependencies without prompting user."
  (interactive)
  (rustic-doc-mode)
  (unless no-dl
    (rustic-doc--install-resources)
    (rustic-doc-install-deps noconfirm))
  (delete-directory (concat rustic-doc-save-loc "/std")
                    t)
  (rustic-doc--start-process "rustic-doc-std-conversion"
                             rustic-doc-convert-prog
                             (lambda (_p)
                               (message "Finished converting docs for std"))
                             "std")
  (if rustic-doc-current-project
      (rustic-doc-convert-current-package)
    (message "Setup is converting std. If you want to convert local dependencies, activate rustic-doc-mode when you are in a rust project and run `rustic-doc-convert-current-package")))

(defun rustic-doc--start-process (name program finish-func &rest program-args)
  "Start a process in buffer `*NAME*' for PROGRAM.
If FINISH-FUNC is non-nil, it will be called after PROGRAM has
exited, with the process object as its only argument.
Any PROGRAM-ARGS are passed to PROGRAM."
  (rustic--inheritenv
   (let* ((buf (generate-new-buffer (concat "*" name "*")))
          (proc (let ((process-connection-type nil))
                  (apply #'start-process name buf program program-args))))
     (set-process-sentinel
      proc (lambda (proc event)
             (let ((buf (process-buffer proc)))
               (if (string-match-p (regexp-quote "abnormally") event)
                   (message "Could not finish process: %s. \
See the *Messages* buffer or %s for more info." event (concat "*" name "*"))
                 (when finish-func
                   (funcall finish-func proc))
                 (when (buffer-live-p buf)
                   (kill-buffer buf))))))
     proc)))



(defun rustic-doc--search-dir (lsp-name short-name)
  "The search directory for documentation.
If short-name was `Option', long-name would be `std::option::Option'.
LSP-NAME is given by the language server, and SHORT-NAME by Emacs.
LSP-NAME is different from the stdlib name.
For example, the LSP-NAME `core::option::Option'
is called `std::option::Option' in the docs."
  (let ((long-name
         (concat (cond
                  ((string-prefix-p "core" lsp-name)
                   (concat "std"
                           (seq-drop lsp-name 4)))
                  ((string-prefix-p "alloc" lsp-name)
                   (concat "std"
                           (seq-drop lsp-name 5)))
                  (t lsp-name))
                 "::"
                 short-name)))
    (rustic-doc--deepest-dir
     (concat (rustic-doc--project-doc-dest)
             "/"
             (seq-reduce (lambda (path p)
                           (concat path "/" p))
                         (split-string long-name "::")
                         "")))))


(defun rustic-doc--thing-at-point-eglot (default)
  "Thing-at-point if using eglot.
If anything goes wrong, return DEFAULT."
  (interactive)
  (if-let ((content (jsonrpc-request
                     (eglot--current-server-or-lose)
                     :textDocument/hover (eglot--TextDocumentPositionParams)))
           ;; text-name is the qualified name, but it sometimes doesn't correspond to the folder structure.
           (text-name (nth 2  (split-string (plist-get  (plist-get content :contents) :value) "\n")))
           (short-name (thing-at-point 'symbol t))
           (search-dir (rustic-doc--search-dir text-name short-name)))
      `((search-dir . ,search-dir)
        (short-name . ,short-name))
    default))

(defun rustic-doc--thing-at-point-lsp-mode (default)
  "Thing at point if using lsp-mode.
If anything goes wrong, return DEFAULT."
  (if-let ((active (boundp 'lsp-mode))
           (lsp-content (when (alist-get 'lsp-mode minor-mode-alist)
                          (-some->> (lsp--text-document-position-params)
                            (lsp--make-request "textDocument/hover")
                            (lsp--send-request)
                            (lsp:hover-contents))))
           ;; `short-name' is the unqalified of a struct, function
           ;; etc, like `Option'
           (short-name (thing-at-point 'symbol t))
           ;; If symbol at point is a primitive, the `value' key is
           ;; different than in most cases.  If it is a primitive, we
           ;; concat the name with primitive for searching.
           (lsp-info (or (nth 1
                              (split-string (gethash "value" lsp-content)))
                         (setq short-name
                               (concat "primitive "
                                       (gethash "value" lsp-content)))))
           (search-dir (rustic-doc--search-dir  lsp-info short-name)))
      `((search-dir . ,search-dir)
        (short-name . ,short-name))
    default))

(defun rustic-doc--thing-at-point ()
  "Return info about `thing-at-point'.
If `thing-at-point' is nil or no language, return defaults."
  (let ((default `((search-dir . ,(rustic-doc--project-doc-dest))
                   (short-name . ,nil))))
    (cond ((boundp 'lsp-mode) (rustic-doc--thing-at-point-lsp-mode default))
          ((boundp 'eglot) (rustic-doc--thing-at-point-eglot default))
          (t  default))))

;;;###autoload
(define-minor-mode rustic-doc-mode
  "Convert rust html docs to .org, and browse the converted docs."
  :lighter " browse rust documentation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-#") 'rustic-doc-search)
            map)
  (dolist (mode '(rust-mode-hook rustic-mode-hook org-mode-hook))
    (add-hook mode 'rustic-doc-mode)))

(provide 'rustic-doc)

;;; rustic-doc.el ends here
