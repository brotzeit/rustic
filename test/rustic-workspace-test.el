;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

(ert-deftest rust-test-workspace-location ()
  (should (equal (rustic-buffer-workspace) default-directory))
  (let* ((test-workspace (expand-file-name "test/test-project/test-workspace/" default-directory))
         (default-directory test-workspace))
    (should (equal (rustic-buffer-workspace) test-workspace))))

;; just test if project-root function works for different versions
(ert-deftest rust-test-project-root ()
  (should (equal (rustic-project-root (project-current)) default-directory)))
