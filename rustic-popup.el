;;; rustic-popup.el --- Cargo popup -*-lexical-binding: t-*-

;;; Commentary:

;; Provides magit like popup.

;;; Code:

(require 'transient)

(require 'rustic-cargo)

(transient-define-prefix rustic-cargo ()
  "Run \"cargo\"."
  ["Environment"
   ("B" rustic-compile-backtrace)]
  ["cargo ..."
   ("g" rustic-recompile
    :description (lambda ()
                   (let ((cmd (or compilation-arguments
                                  rustic-compile-command)))
                     (if (string-prefix-p "cargo " cmd)
                         (substring cmd 6)
                       cmd))))
   ("b" "build"     rustic-cargo-build)
   ("f" "fmt"       rustic-cargo-fmt)
   ("r" "run"       rustic-cargo-run)
   ("c" "clippy"    rustic-cargo-clippy)
   ("o" "outdated"  rustic-cargo-outdated)
   ("e" "clean"     rustic-cargo-clean)
   ("k" "check"     rustic-cargo-check)
   ("t" rustic-cargo-test
    :description (lambda () (format "test %s" rustic-test-arguments)))
   ("d" "doc"       rustic-cargo-doc)]
  (interactive)
  (if (rustic-buffer-crate t)
      (transient-setup 'rustic-cargo)
    (user-error "Not inside a rust project")))

(defclass rustic--compile-backtrace (transient-lisp-variable) ())

(cl-defmethod transient-infix-read ((obj rustic--compile-backtrace))
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (or (cadr (member value choices)) "0")
      (car choices))))

(transient-define-infix rustic-compile-backtrace ()
  :class 'rustic--compile-backtrace
  :variable 'rustic-compile-backtrace
  :description "RUST_BACKTRACE"
  :format " %k %d=%v"
  :choices '("0" "1" "full"))

;;; _
(provide 'rustic-popup)
;;; rustic-popup.el ends here
