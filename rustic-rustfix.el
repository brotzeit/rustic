;;; rustic-rustfix.el --- Support for rustfix         -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements support for `rustfix', a tool that applies
;; the suggestions made by `rustc'.

;;; Code:

(require 'rustic-cargo)

(defvar rustic-rustfix-process-name "rustic-rustfix-process"
  "Process name for rustfix processes.")

(defvar rustic-rustfix-buffer-name "*cargo-rustfix*"
  "Buffer name for rustfix buffers.")

(define-derived-mode rustic-rustfix-mode rustic-compilation-mode "rustfix"
  :group 'rustic)

;;;###autoload
(defun rustic-rustfix ()
  "Run 'cargo fix'."
  (interactive)
  (let* ((command (list (rustic-cargo-bin) "fix" "--allow-dirty"))
         (err-buf rustic-rustfix-buffer-name)
         (proc rustic-rustfix-process-name)
         (mode 'rustic-rustfix-mode))
    (rustic-compilation-process-live)
    (rustic-compilation-start command (list :buffer err-buf :process proc :mode mode))))

;;; _
(provide 'rustic-rustfix)
;;; rustic-rustfix.el ends here
