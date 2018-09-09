;; -*- lexical-binding: t -*-

(ert-deftest rustic-test-format-buffer ()
  (let ((string "fn main()      {}")
        (formatted-string "fn main() {}\n")
        (buf (get-buffer-create "test"))
        (buffer-read-only nil))
    (with-current-buffer buf
      (erase-buffer)
      (should-error (rustic-format-buffer))
      (rustic-mode)
      (insert string)
      (backward-char 10)
      (let ((proc (rustic-format-start-process
                   'rustic-format-sentinel
                   :buffer (current-buffer)
                   :stdin (buffer-string))))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (should (string= (buffer-string) formatted-string))
      (should-not (= (point) (or (point-min) (point-max)))))
    (kill-buffer buf)))

(ert-deftest rustic-test-format-file ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t))
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
                   'rustic-format-file-sentinel
                   nil
                   `(,rustic-rustfmt-bin "/tmp/nofile")))
    (kill-process (get-process rustic-format-process-name))
    (when-let ((proc (get-process rustic-format-process-name)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.01)))))

(ert-deftest rustic-test-format-buffer-before-save ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (buf (get-buffer-create "test"))
         (default-directory org-babel-temporary-directory)
         (file (progn (shell-command-to-string "touch test.rs")
                      (expand-file-name "test.rs")))
         (buffer-read-only nil))
    (let ((rustic-format-on-save t))
      (with-current-buffer buf
        (write-file file)
        (erase-buffer)
        (rustic-mode)
        (insert string)
        (backward-char 10)
        (save-buffer)
        (if-let ((proc (get-process rustic-format-process-name)))
            (while (eq (process-status proc) 'run)
              (sit-for 0.01)))
        (should (string= (buffer-string) formatted-string))
        (should-not (= (point) (or (point-min) (point-max))))))
    (let ((rustic-format-on-save nil))
      (with-current-buffer buf
        (erase-buffer)
        (rustic-mode)
        (insert string)
        (save-buffer)
        (should (string= (buffer-string) (concat string "\n")))))
    (kill-buffer buf)))
