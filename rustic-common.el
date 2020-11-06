;;; rustic-common.el --- Common things -*-lexical-binding: t-*-
;;; Commentary:

;; This library defines things that are shared between different other
;; libraries, which may not be related to one another.  This is a bit
;; of a kludge and it may become unnecessary over time.

;;; Code:
;;; Customization

(defgroup rustic nil
  "Support for Rust code."
  :link '(url-link "https://www.rustic-lang.org/")
  :group 'languages)

(defcustom rustic-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rustic)

(defcustom rustic-rustfmt-config-alist nil
  "An alist of (KEY . VAL) pairs that are passed to rustfmt.

KEY is a symbol that corresponds to a config value of rustfmt.
VALUE is a string, an integer or a boolean."
  :type '(alist :key-type symbol
                :value-type (choice string integer boolean))
  :group 'rustic)

(defcustom rustic-lsp-setup-p t
  "Setup LSP related stuff automatically."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

(defcustom rustic-format-trigger nil
  "Format future rust buffers before saving using rustfmt."
  :type '(choice (const :tag "Format buffer before saving." on-save)
                 (const :tag "Run 'cargo fmt' before compilation." on-compile)
                 (const :tag "Don't format automatically." nil))
  :group 'rustic)

;;; Obsolete

(defvar rustic-format-on-save nil
  "Format rust buffers before saving using rustfmt.")
(make-obsolete 'rustic-format-on-save 'rustic-format-trigger "Rustic 0.19")

(defun rustic-format-on-save-p ()
  "Return non-nil if formatting should happen when saving.
See option `rustic-format-trigger'.  For backward compatibility,
if obsolete `rustic-format-on-save' is non-nil, then also return
non-nil."
  (or rustic-format-on-save (eq rustic-format-trigger 'on-save)))

;;; Essentials

(defun rustic-buffer-workspace (&optional nodefault)
  "Get the workspace root.
If NODEFAULT is t, return nil instead of `default-directory' if directory is
not in a rust project."
  (let ((dir (locate-dominating-file
              (or buffer-file-name default-directory) "Cargo.lock")))
    (if dir
        (expand-file-name dir)
      (if nodefault
          nil default-directory))))

(defun rustic-compute-rustfmt-args ()
  "Compute the arguments to rustfmt from `rustic-rustfmt-config-alist'."
  (let (args)
    (cl-dolist (elem rustic-rustfmt-config-alist args)
      (cl-destructuring-bind (key . val) elem
        (push (format "%s=%s" key (if (booleanp val) (if val "true" "false") val)) args)
        (push "--config" args)))))

;;; Spinner

(require 'spinner)

(defcustom rustic-display-spinner t
  "Display spinner."
  :type 'boolean
  :group 'rustic)

(defcustom rustic-spinner-type 'horizontal-moving
  "Holds the type of spinner to be used in the mode-line.
Takes a value accepted by `spinner-start'."
  :type `(choice (choice :tag "Choose a spinner by name"
                         ,@(mapcar (lambda (c) (list 'const (car c)))
                                   spinner-types))
                 (const :tag "A random spinner" random)
                 (repeat :tag "A list of symbols from `spinner-types' to randomly choose from"
                         (choice :tag "Choose a spinner by name"
                                 ,@(mapcar (lambda (c) (list 'const (car c)))
                                           spinner-types)))
                 (vector :tag "A user defined vector"
                         (repeat :inline t string))))

(defmacro rustic-with-spinner (spinner val mode-line &rest body)
  (declare (indent defun))
  `(when rustic-display-spinner
     (when (spinner-p ,spinner)
       (spinner-stop ,spinner))
     (setq ,spinner ,val)
     (setq mode-line-process ,mode-line)
     ,@body))

;;; _
(provide 'rustic-common)
;;; rustic-common.el ends here
