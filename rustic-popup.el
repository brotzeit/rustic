;;; rustic-popup.el --- Cargo popup -*-lexical-binding: t-*-

;;; Code:

(require 'rustic-compile)

;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom rustic-popup-commands
  '((?b "build"    build)
    (?f "fmt"      fmt)
    (?r "run"      run)
    (?c "clippy"   clippy)
    (?t "test"     test)
    (?o "outdated" outdated)
    (?e "clean"    clean))
  "List of commands that are displayed in the popup.
The first element of each list contains a command's binding."
  :type 'list
  :group 'rustic-popup)

(defface rustic-popup-key-face 
  '((t (:foreground "DarkSeaGreen")))
  "Face used for command shortcuts.")

(defface rustic-popup-section-face
  '((t (:foreground "#f74c00")))
  "Face used for popup section description.")


;;;;;;;;;;;;;;;
;; Popup Mode

(defvar rustic-popup-buffer-name "rustic-popup-buffer"
  "Buffer name for rustic popup buffers.")

(defvar rustic-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'rustic-popup-invoke-popup-action)
    (define-key map (kbd "g") 'rustic-recompile)
    (define-key map (kbd "RET") 'rustic-popup-default-action)
    (define-key map (kbd "q") 'kill-buffer-and-window)
    map)
  "Keymap for rustic popup buffers.")

(define-derived-mode rustic-popup-mode fundamental-mode "RusticPopup"
  "Mode for rustic popup buffers."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local scroll-margin 0))

(defun rustic-popup-insert-backtrace ()
  "Insert backtrace section."
  (let ((inhibit-read-only t)
        (prop (lambda (s)
                (propertize s 'face 'rustic-popup-section-face))))
    (insert (funcall prop "Backtrace: "))
    (cond
     ((string= rustic-compile-backtrace "0")
      (insert " " (funcall prop "0") " | 1 | full"))
     ((string= rustic-compile-backtrace "1")
      (insert " 0 | " (funcall prop "1") " | full"))
     ((string= rustic-compile-backtrace "full")
      (insert " 0 | 1 | " (funcall prop "full")))))
  (insert "\n\n"))

(defun rustic-popup-insert-contents (buf)
  "Insert popup buffer contents."
  (let ((inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (rustic-popup-mode)
      (rustic-popup-insert-backtrace)
      (insert (propertize "Commands: " 'face 'rustic-popup-section-face) "\n")
      (insert " " (propertize "g" 'face 'rustic-popup-key-face)
              "      " "recompile" "   " "\""
              (or compilation-arguments rustic-compile-command)
              "\"" "\n\n")
      (dolist (command rustic-popup-commands)
        (insert "\s")
        (insert (propertize (char-to-string (nth 0 command)) 'face 'rustic-popup-key-face))
        (insert "\s\s\s\s\s\s")
        (insert (nth 1 command))
        (insert "\n"))
      (goto-char (point-min)))))

(defun rustic-popup ()
  "Setup popup."
  (interactive)
  (let ((buf (get-buffer-create rustic-popup-buffer-name))
        (win (split-window-below))
        (inhibit-read-only t))
    (rustic-popup-insert-contents buf)
    (set-window-buffer win buf)
    (select-window win)
    (fit-window-to-buffer)
    (set-window-text-height win (+ (window-height) 1))))


;;;;;;;;;;;;;;;;
;; Interactive

(defun rustic-popup-invoke-popup-action (event)
  "Execute commands which are listed in `rustic-popup-commands'."
  (interactive (list last-command-event))
  (save-excursion
    (let ((char (char-to-string event)))
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward (concat "\s" char "\s"))
          (goto-char (match-beginning 0)))))
    (let* ((command (cadr (split-string
                           (buffer-substring-no-properties
                            (point) (line-end-position)))))
           (c (concat "rustic-cargo-" command)))
      (if (commandp c)
          (call-interactively (intern c))
        (call-interactively 'rustic-compile (concat "cargo-" c))))))

(defun rustic-popup-default-action ()
  "Change backtrace and `compilation-arguments' when executed on 
corresponding line."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (line-beginning-position))
      (cond
       ((looking-at "Backtrace:")
        (cond
         ((string= rustic-compile-backtrace "0")
          (setq rustic-compile-backtrace "1"))
         ((string= rustic-compile-backtrace "1")
          (setq rustic-compile-backtrace "full"))
         ((string= rustic-compile-backtrace "full")
          (setq rustic-compile-backtrace "0")))
        (rustic-popup-insert-contents (current-buffer)))
       ((search-forward-regexp "\srecompile\s" (line-end-position) t)
        (setq compilation-arguments
              (read-string "Compilation arguments: "
                           (or compilation-arguments rustic-compile-command)))
        (rustic-popup-insert-contents (current-buffer)))
       (t
        (message "No default action for line."))))))

(provide 'rustic-popup)
;;; rustic-popup.el ends here
