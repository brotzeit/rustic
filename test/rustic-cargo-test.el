;; -*- lexical-binding: t -*-

(ert-deftest rustic-test-cargo-test ()
  (let* ((string "#[test]
                   fn it_works1() {
                      assert_eq!(2 + 2, 3);
                   }
                   #[test]
                   fn it_works2() {
                      assert_eq!(2 + 2, 3);
                   }")
         (default-directory (rustic-test-count-error-helper string))
         (proc (call-interactively 'rustic-cargo-test))
         (buf (process-buffer proc)))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))
    (should (string= rustic-test-arguments ""))
    (with-current-buffer buf
      ;; check if buffer has correct name and correct major mode
      (should (string= (buffer-name buf) rustic-test-buffer-name))
      (should (eq major-mode 'rustic-cargo-test-mode))
      (should (string-match "it_works1" (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match "it_works2" (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest rustic-test-cargo-test-run ()
  (should (string= rustic-test-arguments ""))
  (let* ((string "#[test]
                   fn it_works1() {
                      assert_eq!(2 + 2, 3);
                   }
                   #[test]
                   fn it_works2() {
                      assert_eq!(2 + 2, 3);
                   }")
         (default-directory (rustic-test-count-error-helper string))
         (proc (rustic-cargo-test-run "it_works2"))
         (buf (process-buffer proc)))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))
    (with-current-buffer buf
      ;; only test it_works2 is supposed to run
      (should-not (string-match "it_works1" (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match "it_works2" (buffer-substring-no-properties (point-min) (point-max)))))))
    
    
  
