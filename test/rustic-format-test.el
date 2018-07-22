;;; rust-format-test.el --- ERT tests for rust-mode.el

(ert-deftest rust-test-format-buffer ()
  (let ((string "fn main()      {}")
        (formatted-string "fn main() {}\n")
        (buf (get-buffer-create "test"))
        (buffer-read-only nil))
    (with-current-buffer buf
      (should-error (rustic-format-buffer))
      (rustic-mode)
      (let ((proc (rustic-format-start-process (current-buffer) 'rustic-format-sentinel string)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (should (string= (buffer-string) formatted-string)))))

(ert-deftest rust-test-format-file ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (default-directory org-babel-temporary-directory)
         (dir (rustic-babel-generate-project))
         (main (expand-file-name "main.rs" (concat dir "/src"))))
    (write-region string nil main nil 0)
    (let ((proc (rustic-format-start-process
                 (current-buffer)
                 'rustic-format-file-sentinel
                 nil
                 `(,rustic-rustfmt-bin ,main))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))
    (with-temp-buffer
      (insert-file-contents main)
      (should (string= (buffer-string) formatted-string)))
    (should-error (rustic-format-start-process
                   (current-buffer)
                   'rustic-format-file-sentinel
                   nil
                   `(,rustic-rustfmt-bin nil)))))
