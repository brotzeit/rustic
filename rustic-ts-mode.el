;;; rustic-ts-mode.el --- use native rust-ts-mode -*-lexical-binding: t-*-
;;; Commentary:

;; Derive from rust-ts-mode instead of rust-mode

;;; Code:

;;;###autoload
(define-derived-mode rustic-mode rust-ts-mode "Rustic"
  "Major mode for Rust code.

\\{rustic-mode-map}"
  :group 'rustic

  (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
    (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t))

  (add-hook 'before-save-hook 'rustic-before-save-hook nil t)
  (add-hook 'after-save-hook 'rustic-after-save-hook nil t))

(provide 'rustic-ts-mode)
;;; rustic-ts-mode.el ends here
