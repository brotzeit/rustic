;;; rust-compile.el --- Compile facilities for rust-mode

;;; Code:

(require 'xterm-color)
(require 'compile)

(defgroup rust-compilation nil
  "Rust Compilation."
  :group 'tools
  :group 'processes)

(defcustom rust-compile--command (purecopy "cargo build")
  "Default command for rust compilation."
  :type 'string
  :group 'rust-compilation)

;;;;;;;;;;;
;; Faces

(defcustom rust-compilation--message-face
  '((t :inherit default))
  "Don't use `compilation-message-face', as ansi colors get messed up."
  :type 'face
  :group 'rust-compilation)

(defcustom rust-compilation--ansi-faces ["black"
                                         "red3"
                                         "green3"
                                         "yellow3"
                                         "blue2"
                                         "magenta3"
                                         "cyan3"
                                         "white"]
  "Term ansi faces."
  :type '(vector string string string string string string string string)
  :group 'rust-compilation)

(define-compilation-mode rust-compilation-mode "rust-compilation"
  "Rust compilation mode."
  (setq buffer-read-only nil)
  (setq-local compilation-message-face rust-compilation--message-face)
  (setq-local xterm-color-names-bright rust-compilation--ansi-faces)
  (setq-local xterm-color-names rust-compilation--ansi-faces))

;;;;;;;;;;;;;
;; Process

(defcustom rust-compile--display-method 'pop-to-buffer
  "Default function used for displaying compilation buffer."
  :type 'function
  :group 'rust-compile)

(defvar rust-compile--process-name "rust-process"
  "Process name for rust compilation processes.")

(defvar rust-compile--buffer-name "*rust-process-buffer*"
  "Buffer name for rust compilation process buffers.")

(defun rust-compile--start-process (command)
  (let ((buf rust-compile--buffer-name)
        (coding-system-for-read 'binary)
        (process-environment (nconc
	                          (list (format "TERM=%s" "ansi"))
                              process-environment)))
    (make-process :name rust-compile--process-name
                  :buffer buf
                  :command command
                  :filter #'shell-process-filter
                  :sentinel #'(lambda (_proc _output)))
    (let ((proc (get-buffer-process buf)))
      (accept-process-output proc 0.1))
    (with-current-buffer buf
      (erase-buffer)
      (rust-compilation-mode)
      (funcall rust-compile--display-method buf))))

(defun shell-process-filter (proc output)
  "Filter for rust compilation process."
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
       (insert (xterm-color-filter output)))))

(defun rust-mode--compile (&optional arg)
  "Compile rust project."
  (interactive "P")
  (let ((command (if arg
                     (read-from-minibuffer "Compile command: ")
                   rust-compile--command))
        (proc (get-process rust-compile--process-name)))
    (if (process-live-p proc)
        (if (yes-or-no-p
             (format "A rust-compile process is running; kill it? "))
            (condition-case ()
                (progn
                  (interrupt-process proc)
                  (sit-for 1)
                  (delete-process proc))
              (error nil))
          (error "Cannot have two processes in `%s' at once"
                 (buffer-name))))
      (rust-compile--start-process (split-string command))))

(provide 'rust-compile)
;;; rust-compile.el ends here

