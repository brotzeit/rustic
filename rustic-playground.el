;;; rustic-playground.el --- Support for playground         -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'org-element)
(require 'url)

;;; Options

(defcustom rustic-playground-url-format "https://play.rust-lang.org/?code=%s"
  "Format string to use when submitting code to the playground."
  :type 'string
  :group 'rustic)

(defcustom rustic-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playground submission."
  :type 'string
  :group 'rustic)

(defcustom rustic-playground-enable-shortener t
  "Enable shortend URL for playground links."
  :type 'boolean
  :safe #'booleanp
  :group 'rustic)

;;; Commands

;;;###autoload
(defun rustic-playground (begin end)
  "Create a shareable URL for the contents of the current region,
src-block or buffer on the Rust playground."
  (interactive "r")
  (let (data)
    (cond
     ((region-active-p)
      (setq data (buffer-substring begin end)))
     ((org-in-src-block-p)
      (setq data (org-element-property :value (org-element-at-point))))
     (t
      (setq data (buffer-substring (point-min) (point-max)))))
    (let* ((escaped-data (url-hexify-string data))
           (playground-url (format rustic-playground-url-format escaped-data))
           (escaped-playground-url (url-hexify-string playground-url)))
      (if (> (length escaped-playground-url) 5000)
          (error "Encoded playground data exceeds 5000 character limit (length %s)"
                 (length escaped-playground-url))
        (let ((shortener-url (format rustic-shortener-url-format escaped-playground-url))
              (url-request-method "POST"))
          (if rustic-playground-enable-shortener
              (url-retrieve shortener-url
                        (lambda (state)
                          ;; filter out the headers etc. included at the
                          ;; start of the buffer: the relevant text
                          ;; (shortened url or error message) is exactly
                          ;; the last line.
                          (goto-char (point-max))
                          (let ((last-line (thing-at-point 'line t))
                                (err (plist-get state :error)))
                            (kill-buffer)
                            (if err
                                (error "Failed to shorten playground url: %s" last-line)
                              (let ((URL (read-from-minibuffer "Playground URL: " last-line)))
                                (browse-url URL))))))
            (browse-url playground-url)))))))


(defun rustic-playground-buffer ()
  "Create a shareable URL for the contents of the buffer on the Rust playground."
  (interactive)
  (rustic-playground (point-min) (point-max)))

(defalias 'rustic-playpen #'rustic-playground)

;;; _
(provide 'rustic-playground)
;;; rustic-playground.el ends here
