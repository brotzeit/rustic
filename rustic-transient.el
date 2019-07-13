;;; rustic-transient.el --- Transient based popup-*-lexical-binding: t-*-

;;; Code:

(require 'rustic-cargo)

;; Load the transient popup only if transient itself is instaled
(if (package-installed-p 'transient)
    (define-transient-command rustic-mode--transient-menu ()
      "Rustic Commands"
      [["Cargo commands"
        ("b" "Build" rustic-cargo-build)
        ("f" "Format" rustic-cargo-fmt)
        ("r" "Run" rustic-cargo-run)
        ("c" "Clippy" rustic-cargo-clippy)
        ("o" "Outdated" rustic-cargo-outdated)
        ("e" "Clean" rustic-cargo-clean)
        ("k" "Check" rustic-cargo-check)
        ("t" "Test" rustic-cargo-test)
        ("q" "Quit" transient-quit-one)
        ]])
  )

(defun rustic--transient-popup ()
  "Invoke the rustic transient popup."
  (interactive)
  (rustic-mode--transient))

(rustic-mode--transient)
(provide 'rustic-transient)
;;; rustic-transient.el ends here
