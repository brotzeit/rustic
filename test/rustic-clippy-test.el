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
           (rustic-cargo-clippy-fix-on-compile t)
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
           ;; (rustic-cargo-clippy-fix-on-compile t)
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
           (rustic-cargo-clippy-fix-on-compile t)
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
