;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

(ert-deftest rust-test-workspace-crate-location ()
  (should (equal (funcall rustic-compile-directory-method) default-directory))
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/")))
    (let ((default-directory (expand-file-name "src" test-crate)))
      (should (string= (rustic-buffer-workspace) test-workspace))
      (should (string= (rustic-buffer-crate) test-crate)))))

;; just test if project-root function works for different versions
(ert-deftest rust-test-project-root ()
  (should (equal (rustic-project-root (project-current)) default-directory)))

;; test if only test-workspace will be compiled with `rustic-buffer-crate'
;; and not another-test-workspace
(ert-deftest rustic-test-rustic-buffer-crate ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/")))
    (let ((default-directory (expand-file-name "src" test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (rustic-test--wait-till-finished rustic-compilation-buffer-name)
        (with-current-buffer buffer
          (should (string-match "Compiling test-crate" (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match "Compiling another-test-crate" (buffer-substring-no-properties (point-min) (point-max)))))))))

;; test if both crates will be compiled with `rustic-buffer-workspace'
(ert-deftest rustic-test-rustic-buffer-workspace ()
  (let* ((test-workspace (expand-file-name "test/test-project/"))
         (test-crate (expand-file-name "test/test-project/crates/test-crate/"))
         (rustic-compile-directory-method 'rustic-buffer-workspace))
    (let ((default-directory (expand-file-name "src" test-crate)))
      (rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          ;; (print (buffer-substring-no-properties (point-min) (point-max)))
          (should (string-match "Compiling test-crate" (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match "Compiling another-test-crate" (buffer-substring-no-properties (point-min) (point-max)))))))))
