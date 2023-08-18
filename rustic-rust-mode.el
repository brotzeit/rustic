;;; rustic-rust-mode.el --- rust-mode related code -*-lexical-binding: t-*-
;;; Commentary:

;; Deprecated code related to rust-mode

;;; Code:

(setq rust-load-optional-libraries nil)
(setq rust-before-save-hook #'rustic-before-save-hook)
(setq rust-after-save-hook #'rustic-after-save-hook)
(require 'rust-mode)

;;; Define aliases for removed rustic functions

(defvaralias 'rustic-indent-offset 'rust-indent-offset)
(defvaralias 'rustic-indent-method-chain 'rust-indent-method-chain)
(defvaralias 'rustic-indent-where-clause 'rust-indent-where-clause)
(defvaralias 'rustic-match-angle-brackets 'rust-match-angle-brackets)
(defvaralias 'rustic-indent-return-type-to-arguments 'rust-indent-return-type-to-arguments)
(defalias 'rustic-indent-line #'rust-mode-indent-line)
(defalias 'rustic-end-of-defun #'rust-end-of-defun)

;;;###autoload
(define-derived-mode rustic-mode rust-mode "Rustic"
  "Major mode for Rust code.

\\{rustic-mode-map}"
  :group 'rustic

  (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
   (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t)))

(provide 'rustic-rust-mode)
;;; rustic-rust-mode.el ends here
