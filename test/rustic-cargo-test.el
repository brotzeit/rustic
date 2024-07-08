;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")
(require 'rustic)
(load (expand-file-name "test-helper.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))))

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
    (rustic-test--wait-till-finished rustic-test-buffer-name)
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
         (proc (rustic-cargo-test-run "it_works2")))
    (rustic-test--wait-till-finished rustic-test-buffer-name)
    (with-current-buffer rustic-test-buffer-name
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
        (rustic-test--wait-till-finished rustic-test-buffer-name)
        (with-current-buffer proc-buf
          (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match "test2" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (setq rustic-test-arguments "")
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
        (rustic-test--wait-till-finished rustic-test-buffer-name)
        (with-current-buffer proc-buf
          (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (setq rustic-test-arguments "")
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
        (rustic-test--wait-till-finished rustic-test-buffer-name)
        (with-current-buffer proc-buf
          (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (setq rustic-test-arguments "")
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
    (rustic-test--wait-till-finished rustic-test-buffer-name)
    (with-current-buffer buf
      (should-not (string-match "^warning:\s" (buffer-substring-no-properties (point-min) (point-max)))))
    (kill-buffer buf))
  (setq rustic-cargo-test-disable-warnings nil))

(ert-deftest rustic-test-cargo-current-test-rerun ()
  (let* ((string "mod tests {
#[test]
fn test1() {
}
#[test]
fn test2() {
}}")
         (default-directory (rustic-test-count-error-helper string))
         (buf (get-buffer-create "test-current-test")))
    (with-current-buffer buf
      (insert string)
      (goto-char (point-min))
      (forward-line 3)
      (let* ((proc (rustic-cargo-current-test))
             (proc-buf (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (let* ((proc (rustic-cargo-test-rerun))
             (proc-buf (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (should (string= rustic-test-arguments "tests::test1"))
      (with-current-buffer proc-buf
        (should (string-match "test1" (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match "test2" (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer proc-buf)))
  (setq rustic-test-arguments "")
  (kill-buffer buf)))

(ert-deftest rustic-test-cargo-run-test-with-mod ()
  (let* ((string "
mod tests1 {
#[test]
fn test11() {
}
}

mod tests2 {
#[test]
fn test21() {
}
}")
         (default-directory (rustic-test-count-error-helper string))
         (buf (get-buffer-create "test-current-test")))
    (with-current-buffer buf
      (insert string)
      (goto-char (point-min))
      (forward-line 1)
      (let* ((proc (rustic-cargo-run-test "tests1"))
             (proc-buf (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))
        (with-current-buffer proc-buf
          (should (string-match "tests1::test1" (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match "test21" (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer proc-buf)))
    (should (= 0 (length (split-string rustic-test-arguments))))
    (kill-buffer buf)))

(ert-deftest rustic-test-build ()
  (let* ((string "fn main() { let s = 1;}")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (rustic-cargo-build-arguments "--all-targets"))
    (with-current-buffer buf
      (call-interactively 'rustic-cargo-build)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))

        (should (string= (s-join " " (process-get proc 'command))
                             (concat (rustic-cargo-bin) " build "
                                     rustic-cargo-build-arguments)))))))

(ert-deftest rustic-test-check ()
  (let* ((string "fn main() { let s = 1;}")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf))))
    (with-current-buffer buf
      (call-interactively 'rustic-cargo-check)
      (let* ((proc (get-process rustic-compilation-process-name))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))

        (should (string= (s-join " " (process-get proc 'command))
                             (concat (rustic-cargo-bin) " check "
                                     rustic-cargo-check-arguments)))

        (with-current-buffer buffer
          (should (string-match "^warning:\s" (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest rustic-cargo-test-test ()
  (let* ((string "mod tests {
#[test]
fn test() {
}
}")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf))))
    (with-current-buffer buf
      (let* ((proc (rustic-cargo-test))
             (buffer (process-buffer proc)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.01))
        (with-current-buffer buffer
          (should (eq major-mode 'rustic-cargo-test-mode)))
        (should (string= (s-join " " (process-get proc 'command))
                         (s-trim (concat (rustic-cargo-bin) " test "
                                    rustic-test-arguments))))))))

(ert-deftest rustic-cargo-expand-test ()
  (let* ((string "fn main() {()}")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf))))
    (rustic-cargo-expand)
    (rustic-test--wait-till-finished rustic-expand-buffer-name)
    (with-current-buffer (get-buffer rustic-expand-buffer-name)
      (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match "^cargo expand" buf-string))))))

(ert-deftest rustic-cargo-login-test ()
  (let* ((process-environment (cl-copy-list process-environment))
         (tempdir (concat (temporary-file-directory) (file-name-as-directory "rustic-cargo-login-test")))
         (credfile (concat tempdir "credentials.toml")))

    (when (file-exists-p credfile) (delete-file credfile))

    (setenv "CARGO_HOME" tempdir)

    (rustic-cargo-login "test-credentials")

    (with-temp-buffer
      (find-file credfile)
      (let ((buf-string (buffer-string)))
        (should (string-match "\\\[registry\\\]\ntoken = \"test-credentials\"\n" buf-string))))))

(provide 'rustic-cargo-test)
