;;; rustic-common.el --- Common things -*-lexical-binding: t-*-
;;; Commentary:

;; This library defines things that are shared between different other
;; libraries, which may not be related to one another.  This is a bit
;; of a kludge and it may become unnecessary over time.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(require 'dash)

(defgroup rustic nil
  "Support for Rust code."
  :link '(url-link "https://www.rustic-lang.org/")
  :group 'languages)

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
