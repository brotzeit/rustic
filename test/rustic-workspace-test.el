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
