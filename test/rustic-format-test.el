;;; rust-format-test.el --- ERT tests for rust-mode.el

(require 'rustic)

(ert-deftest rust-test-format-buffer ()
  (let ((string "fn main()  {}")
        (formatted-string "fn main() {}")
        (buf (get-buffer-create "test"))
        (buffer-read-only nil))
    (with-current-buffer buf
      (should-error (rustic-format-buffer))
      (rustic-mode)
      (insert string)
      (let ((proc (rustic-format-start-process (current-buffer) 'rustic-format-sentinel)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (should (equal (buffer-string) formatted-string)))))
