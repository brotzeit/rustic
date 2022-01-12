;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

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

(ert-deftest rustic-test-cargo-current-test ()
  (let* ((string "#[test]
fn test1() {
}
#[test]
fn test2() {
}")
         (default-directory (rustic-test-count-error-helper string))
         (buf (get-buffer-create "test-current-test")))
    (with-current-buffer buf
      (insert string)
      (goto-char (point-min))
      (forward-line 1)
      (let* ((proc (rustic-cargo-current-test))
             (proc-buf (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (with-current-buffer proc-buf
          (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match "test2" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (kill-buffer buf)))

(ert-deftest rustic-test-cargo-current-test-with-comment ()
  ;; test with comment
  (let* ((string "mod tests {
#[test]
fn test1() {
// test with comment
}
}")
         (default-directory (rustic-test-count-error-helper string))
         (buf (get-buffer-create "test-current-test")))
    (with-current-buffer buf
      (insert string)
      (goto-char (point-min))
      (forward-line 3)
      (let* ((proc (rustic-cargo-current-test))
             (proc-buf (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (with-current-buffer proc-buf
          (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (kill-buffer buf)))

(ert-deftest rustic-test-cargo-current-test-no-test-found ()
  ;; test with use #46
  (let* ((string "mod tests {
#[test]
fn test1() {
use std;
}
}")
         (default-directory (rustic-test-count-error-helper string))
         (buf (get-buffer-create "test-current-test")))
    (with-current-buffer buf
      (insert string)
      (goto-char (point-min))
      (forward-line 3)
      (let* ((proc (call-interactively 'rustic-cargo-current-test))
             (proc-buf (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (with-current-buffer proc-buf
          (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (kill-buffer buf)))

(ert-deftest rustic-test-rustic-cargo-test-disable-warnings ()
  (setq rustic-cargo-test-disable-warnings t)
  (let* ((string "fn main() {
use std;
}
")
         (default-directory (rustic-test-count-error-helper string))
         (proc (call-interactively 'rustic-cargo-test))
         (buf (process-buffer proc)))
    (while (eq (process-status proc) 'run)
      (sit-for 0.1))
    (with-current-buffer buf
      (should-not (string-match "^warning:\s" (buffer-substring-no-properties (point-min) (point-max)))))
    (kill-buffer buf))
  (setq rustic-cargo-test-disable-warnings nil))
