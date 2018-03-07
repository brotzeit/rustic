;;; rust-workspace-test.el --- ERT tests for rust-mode.el

(require 'rust-mode)
(require 'ert)

(ert-deftest rust-test-workspace-location ()
  (should (equal (rust-buffer-workspace) default-directory))
  (let* ((test-workspace (expand-file-name "test/test-project/test-workspace/" default-directory))
         (default-directory test-workspace))
    (should (equal (rust-buffer-workspace) test-workspace))))

;;; rust-workspace-test.el ends here
