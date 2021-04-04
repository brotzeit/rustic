;;; rustic-util.el --- Rust utility functions -*-lexical-binding: t-*-

;;; Commentary:

;; Contains functions for rust tools like rustfmt and RLS.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'package)

(require 'rustic-common)
(require 'rustic-cargo)

;;; _
(provide 'rustic-util)
;;; rustic-util.el ends here
