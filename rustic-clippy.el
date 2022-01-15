;;; rustic-clippy.el --- clippy commands -*-lexical-binding: t-*-
;;; Commentary:

;; This library implements support for `clippy'.

;;; Code:

(require 'rustic-compile)

(defcustom rustic-cargo-clippy-fix-args "--allow-dirty"
  "Default arguments when running 'clippy --fix'."
  :type 'string
  :group 'rustic-cargo)

(defvar rustic-clippy-process-name "rustic-cargo-clippy-process"
  "Process name for clippy processes.")

(defvar rustic-clippy-buffer-name "*cargo-clippy*"
  "Buffer name for clippy buffers.")

(defvar rustic-clippy-arguments ""
  "Holds arguments for 'cargo clippy', similar to `compilation-arguments`.")

(defvar rustic-cargo-clippy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'rustic-cargo-clippy-rerun)
    map)
  "Local keymap for `rustic-cargo-clippy-mode' buffers.")

(define-derived-mode rustic-cargo-clippy-mode rustic-compilation-mode "cargo-clippy"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-clippy-run (&optional args)
  "Run `cargo clippy' with optional ARGS."
  (interactive)
  (let* ((command (list (rustic-cargo-bin) "clippy"))
         (c (append command (split-string (if args args ""))))
         (buf rustic-clippy-buffer-name)
         (proc rustic-clippy-process-name)
         (mode 'rustic-cargo-clippy-mode))
    (rustic-compilation-process-live)
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-clippy (&optional arg)
  "Run 'cargo clippy'.

If ARG is not nil, use value as argument and store it in `rustic-clippy-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clippy-arguments'."
  (interactive "P")
  (rustic-cargo-clippy-run
   (cond (arg
          (setq rustic-clippy-arguments (read-from-minibuffer "Cargo clippy arguments: " rustic-clippy-arguments)))
         ((eq major-mode 'rustic-popup-mode)
          rustic-clippy-arguments)
         (t ""))))

;;;###autoload
(defun rustic-cargo-clippy-rerun ()
  "Run 'cargo clippy' with `rustic-clippy-arguments'."
  (interactive)
  (rustic-cargo-clippy-run rustic-clippy-arguments))

(defun rustic-cargo-clippy-fix ()
  "Run 'clippy fix'."
  (interactive)
  (rustic-cargo-clippy-run
   (concat "--fix "
           (format "%s" rustic-cargo-clippy-fix-args))))

(provide 'rustic-clippy)
;;; rustic-clippy.el ends here
