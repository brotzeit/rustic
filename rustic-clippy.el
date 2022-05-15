;;; rustic-clippy.el --- clippy commands -*-lexical-binding: t-*-
;;; Commentary:

;; This library implements support for `clippy'.

;;; Code:

(require 'rustic-compile)

(defcustom rustic-cargo-clippy-fix-args "--allow-dirty"
  "Default arguments when running 'clippy --fix'."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-default-clippy-arguments "--benches --tests --all-features"
  "Default arguments when running clippy."
  :type 'string
  :group 'rustic-cargo)

(defcustom rustic-lints-arguments "-f custom_lints.toml clippy"
  "Default arguments when running cargo-lints."
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
    (define-key map [remap recompile] 'rustic-cargo-clippy-rerun)
    map)
  "Local keymap for `rustic-cargo-clippy-mode' buffers.")

(define-derived-mode rustic-cargo-clippy-mode rustic-compilation-mode "cargo-clippy"
  :group 'rustic)

;;;###autoload
(defun rustic-cargo-clippy-run (&rest args)
  "Run `cargo clippy' with optional ARGS."
  (interactive)
  (let* ((command (list rustic-cargo-bin "clippy"))
         (params (plist-get args :params))
         (c (append command (split-string (if params params ""))))
         (buf rustic-clippy-buffer-name)
         (proc rustic-clippy-process-name)
         (mode 'rustic-cargo-clippy-mode))
    (unless (plist-get args :no-save)
      (rustic-compilation-process-live))
    (rustic-compilation c (list :buffer buf
                                :process proc
                                :mode mode
                                :sentinel (plist-get args :sentinel)
                                :no-display (plist-get args :silent)))))

;;;###autoload
(defun rustic-cargo-lints ()
  "Run cargo-lints with optional ARGS."
  (interactive)
  (let* ((command `(,(rustic-cargo-bin)
                    "lints"
                    ,@(split-string rustic-lints-arguments)))
         (buf rustic-clippy-buffer-name)
         (proc rustic-clippy-process-name)
         (mode 'rustic-cargo-clippy-mode))
    (rustic-compilation command (list :buffer buf :process proc :mode mode))))

;;;###autoload
(defun rustic-cargo-clippy (&optional arg)
  "Run 'cargo clippy'.

If ARG is not nil, use value as argument and store it in `rustic-clippy-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clippy-arguments'."
  (interactive "P")
  (rustic-cargo-clippy-run
   :params (cond (arg
          (setq rustic-clippy-arguments (read-from-minibuffer "Cargo clippy arguments: " rustic-default-clippy-arguments)))
         ((eq major-mode 'rustic-popup-mode)
          (if (> (length rustic-clippy-arguments) 0)
              rustic-clippy-arguments
            rustic-default-clippy-arguments))
         (t
          rustic-default-clippy-arguments))))

;;;###autoload
(defun rustic-cargo-clippy-rerun ()
  "Run 'cargo clippy' with `rustic-clippy-arguments'."
  (interactive)
  (rustic-cargo-clippy-run rustic-clippy-arguments))

(defun rustic-cargo-clippy-fix (&rest args)
  "Run 'clippy fix'."
  (interactive)
  (rustic-cargo-clippy-run
   :params (concat "--fix "
                   rustic-cargo-clippy-fix-args " "
                   rustic-default-clippy-arguments)
   :no-save (plist-get args :no-save)
   :silent t
   :sentinel (lambda (proc msg)
               (while (eq (process-status proc) 'run)
                 (sit-for 0.1))
               (if (zerop (process-exit-status proc))
                   (kill-buffer (get-buffer rustic-clippy-buffer-name))
                 (funcall rustic-compile-display-method (process-buffer proc))))))

(provide 'rustic-clippy)
;;; rustic-clippy.el ends here
