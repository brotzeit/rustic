;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

;; (ert-deftest rustic-test-crate-path-error ()
;;   (let* ((test-workspace (expand-file-name "test/test-project/"))
;;          (test-crate (expand-file-name "test/test-project/crates/test-crate/")))
;;     (let ((default-directory (expand-file-name "src" test-crate)))
;;       (rustic-cargo-build)
;;       (let* ((proc (get-process rustic-compilation-process-name))
;;              (buffer (process-buffer proc)))
;;         (while (eq (process-status proc) 'run)
;;           (sit-for 0.01))
;;         (with-current-buffer buffer
;;           (goto-char (point-min))
;;           (when (re-search-forward "-->")
;;             (goto-char (match-beginning 0))
;;             (let* ((msg (get-text-property (point) 'compilation-message))
;;                   (loc (compilation--message->loc msg))
;;                   (path (caar (compilation--loc->file-struct loc))))
;;               (with-current-buffer buffer
;;                 (should (string= (concat default-directory path)
;;                                  (concat test-crate "src/lib.rs")))))))))))

;; (ert-deftest rustic-test-workspace-path-error ()
;;   (let* ((test-workspace (expand-file-name "test/test-project/"))
;;          (test-crate (expand-file-name "test/test-project/crates/test-crate/"))
;;          (rustic-compile-directory-method 'rustic-buffer-workspace))

;;     (let ((default-directory (expand-file-name "src" test-crate)))
;;       (rustic-cargo-build)
;;       (let* ((proc (get-process rustic-compilation-process-name))
;;              (buffer (process-buffer proc)))
;;         (while (eq (process-status proc) 'run)
;;           (sit-for 0.01))
;;         (with-current-buffer buffer
;;           (goto-char (point-min))
;;           (when (re-search-forward "-->")
;;             (goto-char (match-beginning 0))
;;             (let* ((msg (get-text-property (point) 'compilation-message))
;;                   (loc (compilation--message->loc msg))
;;                   (path (caar (compilation--loc->file-struct loc))))
;;               (with-current-buffer buffer
;;                 (should (string= (concat default-directory path)
;;                                  (concat test-crate "src/lib.rs"))))
;;               (should (file-exists-p (expand-file-name path test-workspace))))))))))

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
