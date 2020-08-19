;;; rustdoc.el --- Browse rust documentation as .org files -*- lexical-binding: t -*-

;; Copyright (c) 2020 Sam Hedin

;; Author: Sam Hedin <sam.hedin@gmail.com>
;;         Jonas Møller <jonas.moeller2@protonmail.com>
;; URL: https://github.com/samhedin/rustdoc-to-org
;; Version: 0.5
;; Keywords: docs languages
;; Package-Requires: ((emacs "26.1") (helm-ag "0.62") (lsp-mode "7.0") (f "0.20.0"))

;; This file is NOT part of GNU Emacs.

;; MIT License

;; Copyright (c) 2020 Sam Hedin

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package lets you convert rustdoc html-files to org mode files, and lets you browse them with `rustdoc-search'.
;; Run `M-x rustdoc-setup' to download the required files and convert the rust standard library.
;; Run `M-x rustdoc-convert-current-package' to generate and convert docs for the package you are currently visiting.

;;; Code:

(require 'helm-ag)
(require 'url)
(require 'lsp-mode)
(require 'f)

(if (< emacs-major-version 27)
    (defun rustdoc--xdg-data-home ()
      (or (getenv "XDG_DATA_HOME")
          (concat (file-name-as-directory (getenv "HOME"))
                  ".local/share")))
  (progn
    (require 'xdg)
    (fset 'rustdoc--xdg-data-home 'xdg-data-home)))

(defvar rustdoc-lua-filter (concat (file-name-as-directory (getenv "HOME"))
                                   ".local/bin/rustdoc-filter.lua")
  "Save location for the rustdoc lua filter.")

(defvar rustdoc-convert-prog (concat (file-name-as-directory (getenv "HOME"))
                                     ".local/bin/rustdoc-convert.sh")
  "Save location for the rustdoc conversion script.")

(defvar rustdoc-source-user "samhedin")

(defvar rustdoc-source-repo (format "https://raw.githubusercontent.com/%s/rustdoc-to-org/master/"
                                    rustdoc-source-user))

(defvar rustdoc-current-project nil "Location to search for documentation.
All projects and std by default, otherwise last open project and std.")

(defvar rustdoc-save-loc (concat (rustdoc--xdg-data-home)
                                 "/emacs/rustdoc"))

(defvar rustdoc-resources `((,rustdoc-convert-prog
                             (:exec)
                             ,(concat rustdoc-source-repo "convert.sh"))
                            (,rustdoc-lua-filter
                             ()
                             ,(concat rustdoc-source-repo "filter.lua"))))

(defun rustdoc--install-resources ()
  "Install or update the rustdoc resources."
  (dolist (resource rustdoc-resources)
    (pcase resource
      (`(,dst ,opts ,src)
       (condition-case nil
           (progn
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
(defun rustdoc-dumb-search (search-term)
  "Search all projects and std for SEARCH-TERM.
Use this when `rustdoc-search' does not find what you're looking for.
Add `universal-argument' to only search level 1 headers.
See `rustdoc-search' for more information."
  (interactive (let ((short-name (alist-get 'short-name
                                            (rustdoc--thing-at-point))))
                 (list (read-string (format "search term, default %s: " short-name)
                                    nil
                                    nil
                                    short-name))))
  (rustdoc-search search-term t))


;;;###autoload
(defun rustdoc-search (search-term &optional root)
  "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results.
To limit search results to only level 1 headers, add `universal-argument'
Level 1 headers are things like struct or enum names.
if ROOT is non-nil the search is performed from the root dir.
This function tries to be smart and limits the search results
as much as possible. If it ends up being so smart that
it doesn't manage to find what you're looking for, try `rustdoc-dumb-search'."
  (interactive (let ((short-name (alist-get 'short-name
                                            (rustdoc--thing-at-point))))
                 (list (read-string (format "search term, default %s: " short-name)
                                    nil
                                    nil
                                    short-name))))
  ;; These helm-ag settings are to make it work properly with ripgrep.
  (let* ((helm-ag-base-command "rg -L --smart-case --no-heading --color=never --line-number --pcre2")
         (helm-ag-fuzzy-match t)
         (helm-ag-success-exit-status '(0 2))
         (thing-at-point (rustdoc--thing-at-point))
         (short-name (alist-get 'short-name thing-at-point))
         ;; If the user did not accept the default search suggestion, we should not search in that suggestion's directory.
         (search-dir
          (cond
           (root rustdoc-save-loc)
           ((string-equal short-name search-term) (alist-get 'search-dir thing-at-point))
           (t (rustdoc--project-doc-dest))))
         ;; If the prefix arg is provided, we only search for level 1 headers by making sure that there is only one * at the beginning of the line.
         (regex (if current-prefix-arg
                    (progn
                      ;; If current-prefix-arg is not set to nil, helm-ag will pick up the prefix arg too and do funny business.
                      (setq current-prefix-arg nil)
                      "^\\*")
                  "^(?!.*impl)^\\*+"))  ; Do not match if it's an impl
         ;; This seq-reduce turns `enum option' into (kind of) `enum.*option', which lets there be chars between the searched words
         (regexed-search-term (concat regex
                                        ; Regex explanation
                                        ; `-' => Do not match if a return type. A search for Option should not show is_some -> Option
                                        ; `(' => Do not match if it's an argument name.
                                        ; `<' => Do not match if it's a generic type arg
                                      (seq-reduce (lambda (acc s)
                                                    (concat acc "[^-\*(<]*" s))
                                                  (split-string search-term " ")
                                                  ""))))
    (rustdoc--update-current-project)
    (unless (file-directory-p rustdoc-save-loc)
      (rustdoc-setup)
      (message "Running first time setup. Please re-run your search once conversion has completed.")
      (sleep-for 3))
    ;; If the user has not run `rustdoc-convert-current-package' in the current project, we create a default directory that only contains a symlink to std.
    (unless (file-directory-p (rustdoc--project-doc-dest))
      (rustdoc-create-project-dir))
    (condition-case nil
        (helm-ag search-dir regexed-search-term)
      ;; If the search didn't turn anything up we re-run the search in the top level searchdir.
      (error (helm-ag rustdoc-save-loc regexed-search-term)))))

(defun rustdoc--update-current-project ()
  "Update `rustdoc-current-project' if editing a rust file, otherwise leave it."
  (when (and lsp-mode
             (derived-mode-p 'rust-mode 'rustic-mode))
    (setq rustdoc-current-project (lsp-workspace-root))))

(defun rustdoc--deepest-dir (path)
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
    (rustdoc--deepest-dir (f-slash (f-dirname path)))))

(defun rustdoc--project-doc-dest ()
  "The location of the documentation for the current or last seen project.
If the user has not visited a project, returns the main doc directory."
  (if rustdoc-current-project
      (f-join rustdoc-save-loc
              (f-filename rustdoc-current-project))
    rustdoc-save-loc))

;;;###autoload
(defun rustdoc-create-project-dir ()
  "Create a rustdoc arg-directory for the current project. Link with std."
  (let* ((link-tgt (concat (file-name-as-directory (rustdoc--xdg-data-home))
                           "emacs/rustdoc/std"))
         (link-name (concat (rustdoc--project-doc-dest)
                            "/std"))
         (current-doc-dest (rustdoc--project-doc-dest)))
    (if current-doc-dest
        (progn
          (make-directory (rustdoc--project-doc-dest)
                          t)
          (make-symbolic-link link-tgt link-name t))
      (message "Couldn't create project doc directory."))))


;;;###autoload
(defun rustdoc-convert-current-package ()
  "Convert the documentation for a project and its dependencies."
  (interactive)
  (unless (file-directory-p rustdoc-save-loc)
    (rustdoc-setup)
    (message "Running first time setup.")
    (sleep-for 3))
  (if rustdoc-current-project
      (progn
        (message "Converting documentation for %s "
                 rustdoc-current-project)
        (if (/= 0 (call-process "cargo" nil "*cargo-makedocs*" nil "makedocs"))
            (message "cargo makedocs could not generate docs for the current package. See buffer *cargo-makedocs* for more info")
          (let* ((docs-src (concat (file-name-as-directory rustdoc-current-project)
                                   "target/doc"))
                 ;; FIXME: Many projects could share the same docs.
                 ;;        *However* that would have to be versioned, so
                 ;;        we'll have to figure out a way to coerce `<crate>-<version>`
                 ;;        strings out of cargo, or just parse the Cargo.toml file, but
                 ;;        then we'd have to review different parsing solutions.
                 (finish-func (lambda (_p)
                                (message (format "Finished converting docs for %s"
                                                 rustdoc-current-project)))))
            (rustdoc-create-project-dir)
            (async-start-process "rustdoc-convert"
                                 rustdoc-convert-prog
                                 finish-func
                                 docs-src
                                 (rustdoc--project-doc-dest)))))
    (message "Could not find project to convert. Visit a rust project first! (Or activate rustdoc-mode if you are in one)")))

;;;###autoload
(defun rustdoc-setup ()
  "Setup or update rustdoc filter and convert script. Convert std."
  (interactive)
  (rustdoc--install-resources)
  (message "Setup is converting the standard library")
  (delete-directory (concat rustdoc-save-loc "/std")
                    t)
  (async-start-process "*rustdoc-std-conversion*"
                       rustdoc-convert-prog
                       (lambda (_p)
                         (message "Finished converting docs for std"))
                       "std"))

(defun rustdoc--thing-at-point ()
  "Return info about `thing-at-point'. If `thing-at-point' is nil, return defaults."
  (if-let ((active lsp-mode)
           (lsp-content (-some->> (lsp--text-document-position-params)
                          (lsp--make-request "textDocument/hover")
                          (lsp--send-request)
                          (lsp:hover-contents)))
           ;; `short-name' is the unqalified of a struct, function etc, like `Option'
           (short-name (thing-at-point 'symbol t))
           ;; If symbol at point is a primitive, the `value' key is different than in most cases.
           ;; If it is a primitive, we concat the name with primitive for searching.
           (lsp-info (or (nth 1
                              (split-string (gethash "value" lsp-content)))
                         (setq short-name (concat "primitive "
                                                  (gethash "value" lsp-content)))))
           ;; If short-name was `Option', long-name would be `std::option::Option'
           (long-name (concat (cond
                               ((string-prefix-p "core" lsp-info)
                                (concat "std"
                                        (seq-drop lsp-info 4)))
                               ((string-prefix-p "alloc" lsp-info)
                                (concat "std"
                                        (seq-drop lsp-info 5)))
                               (t lsp-info))
                              "::"
                              short-name))
           (search-dir (rustdoc--deepest-dir (concat (rustdoc--project-doc-dest)
                                                     "/"
                                                     (seq-reduce (lambda (path p)
                                                                   (concat path "/" p))
                                                                 (split-string long-name "::") "")))))
      `((search-dir . ,search-dir)
        (short-name . ,short-name))
    `((search-dir . ,(rustdoc--project-doc-dest))
      (short-name . ,nil))))

;;;###autoload
(define-minor-mode rustdoc-mode
  "Convert rust html docs to .org, and browse the converted docs."
  :lighter " browse rust documentation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-#") 'rustdoc-search)
            map)
  (dolist (mode '(rust-mode-hook rustic-mode-hook org-mode-hook))
    (add-hook mode 'rustdoc-mode))
  (rustdoc--update-current-project))

(provide 'rustdoc)

;;; rustdoc.el ends here
