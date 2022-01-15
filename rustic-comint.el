;;; rustic-comint.el --- Cargo and comint facilities -*-lexical-binding: t-*-
;;; Commentary:

;; This implements various functionalities related to cargo run and
;; comint interaction.

;;; Code:

(require 'comint)

(require 'rustic-cargo)
(require 'rustic-compile)

;;; Customization

(defcustom rustic-cargo-run-use-comint nil
  "If t then interact with programs in `rustic-cargo-run' using
comint-mode.  This creates a dependency on the polymode package.
No special configuration of polymode is needed for this to work,
but you need to install polymode separately."
  :type 'boolean
  :group 'rustic-cargo)

;;; Run with comint

(defvar rustic-run-comint-process-name "rustic-cargo-run-comint-process"
  "Process name for run-comint processes.")

(defvar rustic-run-comint-buffer-name "*cargo-run-comint*"
  "Buffer name for run-comint buffers.")

(defvar rustic-run-comint-arguments ""
  "Holds arguments for 'cargo run-comint', similar to `compilation-arguments`.")

(defvar rustic-cargo-run-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g") 'rustic-cargo-comint-run-rerun)
    map)
  "Local keymap for `rustic-cargo-comint-mode' buffers.")

(define-derived-mode rustic-cargo-run-comint-mode comint-mode "Cargo comint"
  "Mode for 'cargo run' that derives from `comint-mode'.

To send input to the compiled program, just type in a string and
hit RET to send it to the program."
  (buffer-disable-undo)
  (setq buffer-read-only nil))

;;;###autoload
(defun rustic-cargo-comint-run (&optional arg)
  "Run 'cargo run' but for interactive programs.

If ARG is not nil, use value as argument and store it in `rustic-run-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-run-arguments'."
  (interactive "P")
  (pop-to-buffer-same-window
   (get-buffer-create rustic-run-comint-buffer-name))
  (unless (comint-check-proc (current-buffer))
    (rustic--cargo-repl-in-buffer
     (current-buffer)
     (concat "run" (cond
                    (arg
                     (setq rustic-run-comint-arguments
                           (read-from-minibuffer "Cargo run arguments: " rustic-run-comint-arguments)))
                    ((rustic--get-run-arguments))
                    (t ""))))
    (rustic-cargo-run-comint-mode)))

;;;###autoload
(defun rustic-cargo-comint-run-rerun ()
  "Run 'cargo run' with `rustic-run-comint-arguments'."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create rustic-run-comint-buffer-name))
  (rustic--cargo-repl-in-buffer
   (current-buffer)
   (concat "run" rustic-run-comint-arguments)))

(defun rustic--cargo-repl-in-buffer (buffer run-args)
  "Make Cargo comint Repl in BUFFER.
Optionally accepts RUN-ARGS which will be passed to the
executable."
  (make-comint-in-buffer
   rustic-run-comint-buffer-name
   buffer
   (rustic-cargo-bin)
   '()
   run-args))

;;; Cargo run with plain comint and optional polymode

(defun rustic-cargo-plainrun-mode ()
  (interactive)
  (if rustic-cargo-run-use-comint
      ;; rustic-cargo-comint-run-mode toggles the mode; we want to
      ;; always enable.
      (unless (and (boundp 'polymode-mode)
                   polymode-mode
                   (memq major-mode '(rustic-cargo-plain-run-mode
                                      comint-mode)))
        (rustic-cargo-comint-run-mode))
    (rustic-cargo-plain-run-mode)))

;;;###autoload
(defun rustic-cargo-plain-run (&optional arg)
  "Run 'cargo run' for the current project.
If running with prefix command `C-u', read whole command from minibuffer."
  (interactive "P")
  (let* ((command (if arg
                      (read-from-minibuffer "Cargo run command: " "cargo run -- ")
                    (concat (rustic-cargo-bin) " run "
                            (setq rustic-run-arguments
                                  (read-from-minibuffer
                                   "Run arguments: "
                                   (if (rustic-cargo-run-get-relative-example-name)
                                       (concat "--example "
                                               (rustic-cargo-run-get-relative-example-name))
                                     (car compile-history))
                                   nil nil 'compile-history)) ))))
    (rustic-run-cargo-command command (list :mode 'rustic-cargo-plainrun-mode))))

(define-derived-mode rustic-cargo-plain-run-mode rustic-compilation-mode "Cargo run"
  "Mode for 'cargo run' that derives from `rustic-compilation-mode'.
To send input to the compiled program, use
`rustic-compile-send-input'.  If you set
`rustic-cargo-run-use-comint' to t, you can also just type in a
string and hit RET to send it to the program.  The latter
approach requires installing polymode."
  (buffer-disable-undo)
  (setq buffer-read-only nil)
  (use-local-map comint-mode-map))

(defun rustic-cargo-comint-run-mode ()
  "Mode for 'cargo run' that combines `rustic-compilation-mode' with `comint-mode',
the former for highlighting and interacting with compiler errors,
and the latter for interacting with the compiled program."
  ;; First time around, define the mode and invoke it.  Next time, the
  ;; symbol will have been overwritten so this runs only once.
  (unless (require 'polymode nil 'noerr)
    (error "polymode not found; polymode must be installed for `rustic-cargo-run-use-comint' to work"))
  (let ((docstr (documentation 'rustic-cargo-comint-run-mode)))
    (define-hostmode poly-rustic-cargo-compilation-hostmode
      :mode 'rustic-cargo-plain-run-mode)
    (define-innermode poly-rustic-cargo-comint-innermode
      :mode 'comint-mode
      :head-matcher "^ *Running `.+`$"
      :head-mode 'host
      :tail-matcher "\\'"
      :tail-mode 'host)
    (define-polymode rustic-cargo-comint-run-mode
      :hostmode 'poly-rustic-cargo-compilation-hostmode
      :innermodes '(poly-rustic-cargo-comint-innermode)
      :switch-buffer-functions '(poly-rustic-cargo-comint-switch-buffer-hook)

      ;; See comments in poly-rustic-cargo-comint-switch-buffer-hook below.
      (set (make-local-variable 'pm-hide-implementation-buffers) nil)
      )
    (put 'rustic-cargo-comint-run-mode 'function-documentation docstr)
    (rustic-cargo-comint-run-mode)))

(defun poly-rustic-cargo-comint-switch-buffer-hook (old-buffer new-buffer)
  "Housekeeping for `rustic-cargo-comint-run-mode'."
  ;; Keep inferior process attached to the visible buffer.
  (let ((proc (get-buffer-process old-buffer)))
    (when proc
      (set-process-buffer proc new-buffer)))
  ;; Prevent polymode from constantly renaming the
  ;; "*rustic-compilation*" buffer.  A note in case this undocumented
  ;; variable stops working: if that happens, you'll see
  ;; *rustic-compilatin*[comint]<2>, <3>, etc. keep popping up.
  (set (make-local-variable 'pm-hide-implementation-buffers) nil))

(provide 'rustic-comint)
;;; rustic-comint.el ends here
