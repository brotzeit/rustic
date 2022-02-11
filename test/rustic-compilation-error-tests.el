;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

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
  ;; NOTE: this doesn't seem to be the case anymore
  ;; compilation-num-errors-found would be 8 with regular compilation mode
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

(ert-deftest rustic-test-count-warnings ()
  (let* ((string "fn main() {
                       let v1 = vec![1, 2, 3];
                       let v2 = vec![1, 2, 3];
                       let v3 = vec![1, 2, 3];
                       let v4 = vec![1, 2, 3];
                       let v5 = vec![1, 2, 3];
                       let v6 = vec![1, 2, 3];
                       let v7 = vec![1, 2, 3];
                       let v8 = vec![1, 2, 3];
                       let v9 = vec![1, 2, 3];
                       let v10 = vec![1, 2, 3];
                       let v11 = vec![1, 2, 3];
                       let v12 = vec![1, 2, 3];
                       let v13 = vec![1, 2, 3];
                       let v14 = vec![1, 2, 3];
                       let v15 = vec![1, 2, 3];
                       let v16 = vec![1, 2, 3];
                       let v17 = vec![1, 2, 3];
                       let v18 = vec![1, 2, 3];
                       let v19 = vec![1, 2, 3];
                       let v20 = vec![1, 2, 3];
                    }")
         (default-directory (rustic-test-count-error-helper string))
         (proc (rustic-compilation-start (split-string "cargo build"))))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))
    (with-current-buffer (get-buffer rustic-compilation-buffer-name)
      (should (= compilation-num-warnings-found 20)))))
(ert-deftest rustic-test-crate-path-error ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/")))
    (let ((default-directory (expand-file-name "src" test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "-->")
            (goto-char (match-beginning 0))
            (let* ((msg (get-text-property (point) 'compilation-message))
                  (loc (compilation--message->loc msg))
                  (path (caar (compilation--loc->file-struct loc))))
              (with-current-buffer buffer
                (should (string= (concat default-directory path)
                                 (concat test-crate "src/lib.rs")))))))))))

;; TODO: improve error testing for crates
(ert-deftest rustic-test-workspace-path-error ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/"))
         (another-test-crate (expand-file-name "test/test-project/crates/another-test-crate/"))
         (rustic-compile-directory-method 'rustic-buffer-workspace))

    (let ((default-directory (expand-file-name "src" test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "-->")
            (goto-char (match-beginning 0))
            (let* ((msg (get-text-property (point) 'compilation-message))
                   (loc (compilation--message->loc msg))
                   (path (caar (compilation--loc->file-struct loc))))
              (with-current-buffer buffer
                (should (or (string= (concat default-directory path)
                                     (concat test-crate "src/lib.rs"))
                            (string= (concat default-directory path)
                                     (concat another-test-crate "src/lib.rs"))))
                (should (file-exists-p (expand-file-name path test-workspace)))))))))))

;; in this test we check if goto error works a crate depends on another crate in
;; the same workspace which contains an error
(ert-deftest rustic-test-crate-local-dependency-error ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/"))
         (depend-on-test-crate (expand-file-name "test/test-project/crates/depend-on-test-crate/")))

    (let ((default-directory (expand-file-name "src" depend-on-test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (goto-char (point-min))
          (compilation-next-error 1)
          (should (string= default-directory test-workspace))
          (let* ((msg (get-text-property (point) 'compilation-message))
                 (loc (compilation--message->loc msg))
                 (path (caar (compilation--loc->file-struct loc))))
            (should (string= default-directory test-workspace))
            (should (string= (concat default-directory path)
                             (concat test-crate "src/lib.rs")))))))))



(ert-deftest rustic-test-compilation-scroll-output-with-rustic-crate ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/"))
         (depend-on-test-crate (expand-file-name "test/test-project/crates/depend-on-test-crate/"))
         (compilation-scroll-output 'first-error))
    (let ((default-directory (expand-file-name "src" depend-on-test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (should (string= default-directory test-workspace))
          (let* ((msg (get-text-property (point) 'compilation-message))
                 (loc (compilation--message->loc msg))
                 (path (caar (compilation--loc->file-struct loc))))
            (should (string= default-directory test-workspace))
            (should (string= (concat default-directory path)
                             (concat test-crate "src/lib.rs")))))))))

(ert-deftest rustic-test-compilation-scroll-output-with-rustic-workspace ()
  (let* ((test-workspace (expand-file-name "test/test-project-single-crate/"))
         (test-crate (expand-file-name "test/test-project-single-crate/crates/test-crate/"))
         (rustic-compile-directory-method 'rustic-buffer-workspace)
         (compilation-scroll-output 'first-error))
    (let ((default-directory (expand-file-name "src" test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (should (string= default-directory test-workspace))
          (let* ((msg (get-text-property (point) 'compilation-message))
                 (loc (compilation--message->loc msg))
                 (path (caar (compilation--loc->file-struct loc))))
            (should (string= default-directory test-workspace))
            (should (string= (concat default-directory path)
                             (concat test-crate "src/lib.rs")))))))))

(ert-deftest rustic-test-compilation-scroll-nil ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/"))
         (depend-on-test-crate (expand-file-name "test/test-project/crates/depend-on-test-crate/"))
         (rustic-compile-directory-method 'rustic-buffer-crate)
         (compilation-scroll-output nil))
    (let ((default-directory (expand-file-name "src" depend-on-test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (should (string= default-directory test-workspace))
          (should-not (get-text-property (point) 'compilation-message)))))))
