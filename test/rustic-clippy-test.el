;; -*- lexical-binding: t -*-


(ert-deftest rustic-test-trigger-and-fix-format-on-compile ()
  (ignore-errors (kill-buffer (get-buffer rustic-compilation-buffer-name)))
  (let* ((buffer1 (get-buffer-create "b1"))
         (string "fn main()  { String::from(' ').extend(String::from(' ').chars()); }    ")
         (formatted-string "fn main() {\n    String::from(' ').push_str(&String::from(' '));\n}\n")
         (dir (rustic-babel-generate-project t))
         (compilation-read-command nil))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           (rustic-cargo-clippy-trigger-fix 'on-compile)
           (rustic-format-trigger 'on-compile))
      (with-current-buffer buffer1
        (insert string)
        (write-file file1))

      (if-let ((proc (call-interactively 'rustic-cargo-build)))
          (while (eq (process-status proc) 'run)
            (sit-for 0.01)))

      (with-current-buffer buffer1
        (revert-buffer t t)
        (should (string= (buffer-string) formatted-string)))
      (should-not (get-buffer rustic-clippy-buffer-name))
      (kill-buffer buffer1))))

(ert-deftest rustic-test-fix-format-on-compile-default-and-format ()
  (ignore-errors (kill-buffer (get-buffer rustic-compilation-buffer-name)))
  (let* ((buffer1 (get-buffer-create "b1"))
         (string "fn main()     { String::from(' ').extend(String::from(' ').chars()); }    ")
         (formatted-string "fn main() {
    String::from(' ').extend(String::from(' ').chars());
}\n")
         (dir (rustic-babel-generate-project t))
         (compilation-read-command nil))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           ;; (rustic-cargo-clippy-trigger-fix 'on-compile)
           (rustic-format-trigger 'on-compile))
      (with-current-buffer buffer1
        (insert string)
        (write-file file1))

      (if-let ((proc (call-interactively 'rustic-compile)))
          (while (eq (process-status proc) 'run)
            (sit-for 0.01)))

      (with-current-buffer buffer1
        (revert-buffer t t)
        (should (string= (buffer-string) formatted-string)))
      (should-not (get-buffer rustic-clippy-buffer-name))
      (kill-buffer buffer1))))

(ert-deftest rustic-test-trigger-and-fix-format-on-compile-clippy-error ()
  (ignore-errors (kill-buffer (get-buffer rustic-clippy-buffer-name)))
  (let* ((buffer1 (get-buffer-create "b1"))
         (string "ffn main()  { String::from(' ').extend(String::from(' ').chars()); }    ")
         (formatted-string "fn main() {\n    String::from(' ').push_str(&String::from(' '));\n}\n")
         (dir (rustic-babel-generate-project t))
         (compilation-read-command nil))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           (rustic-cargo-clippy-trigger-fix 'on-compile)
           (rustic-format-trigger 'on-compile))
      (with-current-buffer buffer1
        (insert string)
        (write-file file1))

      (if-let ((proc (call-interactively 'rustic-cargo-build)))
          (while (eq (process-status proc) 'run)
            (sit-for 0.5)))

      (with-current-buffer buffer1
        (revert-buffer t t)
        (should-not (string= (buffer-string) formatted-string)))
      (should (get-buffer rustic-clippy-buffer-name))
      (kill-buffer (get-buffer rustic-clippy-buffer-name))
      (kill-buffer buffer1))))

(ert-deftest rustic-test-clippy ()
  (let* ((string "fn main() {
     String::from(' ').extend(String::from(' ').chars());
}\n")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf))))
    (call-interactively 'rustic-cargo-clippy)
    (let* ((proc (get-process rustic-clippy-process-name))
           (buffer (process-buffer proc)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.01))
      (with-current-buffer buffer
        (should (string-match "^warning:\s" (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string= (s-join " " (process-get proc 'command))
                     (concat (rustic-cargo-bin) " clippy "
                             rustic-default-clippy-arguments))))
    (should (string= rustic-clippy-arguments ""))

    (kill-buffer (get-buffer rustic-clippy-buffer-name))
    (call-interactively 'rustic-cargo-clippy-rerun)
    (let* ((proc (get-process rustic-clippy-process-name))
           (buffer (process-buffer proc)))
      (rustic-test--wait-till-finished rustic-clippy-buffer-name)
      (with-current-buffer buffer
        (should (string-match "^warning:\s" (buffer-substring-no-properties (point-min) (point-max)))))
      (should (string= rustic-clippy-arguments "")))
    (kill-buffer buf)))

(ert-deftest rustic-test-clippy-fix ()
  (let* ((string "fn main() { let s = 1;}")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf))))
    (with-current-buffer buf
      (call-interactively 'rustic-cargo-clippy-fix)
      (let* ((proc (get-process rustic-clippy-process-name))
             (buffer (process-buffer proc)))
        (rustic-test--wait-till-finished rustic-clippy-buffer-name)
        (revert-buffer t t)
        (should (string= (buffer-string) "#![allow(non_snake_case)]\nfn main() { let _s = 1;}"))))))
