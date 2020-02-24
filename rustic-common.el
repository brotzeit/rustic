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

;;; _
(provide 'rustic-common)
;;; rustic-common.el ends here
