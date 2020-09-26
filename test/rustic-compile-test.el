;; -*- lexical-binding: t -*-

(ert-deftest rustic-test-format-next-error-last-buffer ()
  (let ((string "fn main()      {}")
        (buf (get-buffer-create "test-next-error-last-buffer")))
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
        (with-current-buffer (process-buffer proc)
          (should (eq next-error-last-buffer buf)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (sit-for 0.5)))))

(ert-deftest rustic-test-save-some-buffers ()
  (let* ((buffer1 (get-buffer-create "b1"))
         (buffer2 (get-buffer-create "b2"))
         (string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t))
         (rustic-format-trigger 'on-save))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           (file2 (progn (shell-command-to-string "touch src/test.rs")
                         (expand-file-name "test.rs" src))))
      (with-current-buffer buffer1
        (write-file file1)
        (insert string))
      (with-current-buffer buffer2
        (write-file file2)
        (insert string))
      (let ((buffer-save-without-query t))
        (rustic-save-some-buffers))
      (sit-for 1)
      (with-current-buffer buffer1
        (should (string= (buffer-string) formatted-string)))
      (with-current-buffer buffer2
        (should (string= (buffer-string) formatted-string))))
    (kill-buffer buffer1)
    (kill-buffer buffer2)))

(ert-deftest rustic-test-compile ()
  (let* ((dir (rustic-babel-generate-project t)))
    (should-not compilation-directory)
    (should-not compilation-arguments)
    (setq compilation-arguments "cargo fmt")
    (let* ((default-directory dir)
           (compilation-read-command nil)
           (proc (rustic-compile)))
      (should (process-live-p proc))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (should (string= compilation-directory dir))
      (let ((proc (rustic-recompile)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (should (string= compilation-arguments "cargo build"))
      (should (string= compilation-directory dir))))
  (setq compilation-directory nil)
  (setq compilation-arguments nil))

(ert-deftest rustic-test-recompile ()
  (let ((rustic-format-trigger nil)
        (compilation-read-command nil))
    (let ((proc (rustic-compile)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (should (= 101 (process-exit-status proc))))
    (let* ((dir (rustic-babel-generate-project t))
           (default-directory dir)
           (proc (rustic-compile)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (should (= 0 (process-exit-status proc))))
    ;; now don't set default-directory for rustic-recompile
    (let ((proc (rustic-recompile)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      ;; (print (process-exit-status proc))
      (should (= 0 (process-exit-status proc))))))

(ert-deftest rustic-test-backtrace ()
  (let* ((string "fn main() {
                       let v = vec![1, 2, 3];
                       v[99];
                    }")
         (default-directory (rustic-test-count-error-helper string)))
    (let ((rustic-compile-backtrace "0")
          (proc (rustic-compilation-start (split-string "cargo run"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))
    (let ((rustic-compile-backtrace "1")
          (proc (rustic-compilation-start (split-string "cargo run"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))
    (let ((rustic-compile-backtrace "full")
          (proc (rustic-compilation-start (split-string "cargo run"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))))

(ert-deftest rustic-test-count-errors ()
  ;; test error without error code
  (let* ((string "fn main() {")
         (default-directory (rustic-test-count-error-helper string))
         (rustic-format-trigger nil))
    (let ((proc (rustic-compilation-start (split-string "cargo build"))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-compilation-buffer-name)
        (should (= compilation-num-errors-found 1))))))

(ert-deftest rustic-test-cargo-test ()
  ;; compilation-num-errors-found would be 8 with regular compilaton mode
  ;; due to parsing issues https://github.com/rust-lang/rust-mode/pull/254
  (let ((rustic-compile-backtrace "full"))
    (let* ((string "#[cfg(test)]
                  mod tests {
                      #[test]
                      fn it_works() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works2() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works3() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works4() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works5() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works6() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works7() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works8() {
                          assert_eq!(2 + 2, 3);
                      }

                      // compile.el stops parsing here

                      #[test]
                      fn it_works9() {
                          assert_eq!(2 + 2, 3);
                      }
                      #[test]
                      fn it_works10() {
                          assert_eq!(2 + 2, 3);
                      }
                  }")
           (default-directory (rustic-test-count-error-helper string))
           (proc (rustic-cargo-test)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1))
      (with-current-buffer (get-buffer rustic-test-buffer-name)
        (should (= compilation-num-errors-found 10))))))

;; ;; TODO: parsing doesn't work
;; (ert-deftest rustic-test-count-warnings ()
;;   (let* ((string "fn main() {
;;                        let v1 = vec![1, 2, 3];
;;                        let v2 = vec![1, 2, 3];
;;                     }")
;;          (default-directory (rustic-test-count-error-helper string))
;;          (proc (rustic-compilation-start (split-string "cargo build"))))
;;     (while (eq (process-status proc) 'run)
;;       (sit-for 0.1))
;;     (with-current-buffer (get-buffer rustic-compilation-buffer-name)
;;       (should (= compilation-num-warnings-found 1)))))




