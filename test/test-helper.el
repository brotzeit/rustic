;;; test-helper.el --- Helpers for rust-mode-reloaded-test.el

(require 'ert)
(require 'f)

(let ((rust-mode-dir (f-parent (f-dirname (f-this-file)))))
(add-to-list 'load-path rust-mode-dir))

;;; test-helper.el ends here
