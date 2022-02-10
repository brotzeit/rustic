;;; rustic-expand.el --- Expand support -*-lexical-binding: t-*-
;;; Commentary:

;; This implements various functionalities related to cargo expand to
;; show result of expansion

;;; Code:

(require 'rustic-cargo)
(require 'rustic-compile)

(defvar rustic-expand-process-name "rustic-cargo-expand-process"
  "Process name for expand processes.")

(defvar rustic-expand-buffer-name "*cargo-expand*"
  "Buffer name for expand buffers.")

(defvar rustic-expand-arguments ""
  "Holds arguments for 'cargo expand', similar to `compilation-arguments`.")

(defvar rustic-cargo-expand-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'rustic-cargo-expand-rerun)
    map)
  "Local keymap for `rustic-cargo-expand-mode' buffers.")

(define-derived-mode rustic-cargo-expand-mode rustic-compilation-mode "cargo-expand"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-expand (&optional arg)
  "Run 'cargo expand'.

If ARG is not nil, use value as argument and store it in
`rustic-expand-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-expand-arguments'."
  (interactive "P")
  (rustic-cargo-expand-command
   (cond (arg
          (setq rustic-expand-arguments (read-from-minibuffer "Cargo expand arguments: " rustic-expand-arguments)))
         (t ""))))

(defun rustic-cargo-expand-command (&optional expand-args)
  "Start compilation process for 'cargo expand' with optional EXPAND-ARGS."
  (rustic-compilation-process-live)
  (let* ((command (list (rustic-cargo-bin) "expand"))
         (c (append command (split-string (if expand-args expand-args ""))))
         (buf rustic-expand-buffer-name)
         (proc rustic-expand-process-name)
         (mode 'rustic-cargo-expand-mode))
    (rustic-compilation c (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-expand-rerun ()
  "Run 'cargo expand' with `rustic-expand-arguments'."
  (interactive)
  (rustic-cargo-expand-command rustic-expand-arguments))

(provide 'rustic-expand)
;;; rustic-expand.el ends here
