;; -*- lexical-binding: t -*-

(setq org-confirm-babel-evaluate nil)

(defun rustic-test-get-babel-block (contents)
  "Return buffer containing babel block with CONTENTS."
  (let ((buf (get-buffer-create "babel-test")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+BEGIN_SRC rustic\n")
      (insert contents)
      (insert "\n#+END_SRC")
      buf)))

(defun rustic-test-babel-wait ()
  "Wait for babel results."
  (sit-for 2)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+RESULTS:\n: cargo" nil t)
      (goto-char (point-min))
      (sit-for 0.5))))

(defun rustic-test-babel-execute-block (buf &optional nowait)
  "Execute babel block in BUF."
  (with-current-buffer buf
    (call-interactively 'org-ctrl-c-ctrl-c)
    (unless nowait
      (rustic-test-babel-wait))))

(defun rustic-test-babel-check-results (string buf)
  "Returns t if babel result in BUF equals STRING."
  (let (result)
    (with-current-buffer buf
      (goto-char (point-min))
      (when (search-forward "#+RESULTS:\n:\s" nil t)
        (goto-char (match-end 0))
        (setq result (buffer-substring-no-properties (point) (point-max)))))
    (string= string result)))

(ert-deftest rustic-test-babel-result ()
  (let* ((string "fn main() {
                    println!(\"{}\", \"foo\");
                  }")
         (buf (rustic-test-get-babel-block string)))
    (rustic-test-babel-execute-block buf)
    (should (rustic-test-babel-check-results "foo\n" buf))
    (should-not (buffer-live-p rustic-babel-compilation-buffer-name)))
  (let* ((string "fn main() {
                    let foo = 1;
                  }")
         (buf (rustic-test-get-babel-block string)))
    (rustic-test-babel-execute-block buf)
    (should (rustic-test-babel-check-results nil buf))
    (should-not (buffer-live-p rustic-babel-compilation-buffer-name))))

(ert-deftest rustic-test-babel-spinner ()
  (let* ((string "fn main() {
                    use std::{thread, time};
                    let ten_millis = time::Duration::from_millis(2000);
                    let now = time::Instant::now();
                    thread::sleep(ten_millis);
                  }")
         (buf (rustic-test-get-babel-block string)))
    (with-current-buffer buf
      (should-not (spinner-p rustic-babel-spinner))
      (should (eq mode-line-process nil))
      (rustic-test-babel-execute-block buf t)
      (should (spinner-p rustic-babel-spinner))
      (should-not (eq mode-line-process nil))
      (goto-char (point-min))
      (while (search-forward "#+RESULTS:\n: cargo" nil t)
        (should (spinner-p rustic-babel-spinner))
        (should-not (eq mode-line-process nil))
        (goto-char (point-min))
        (sit-for 0.1))
      (should-not (spinner-p rustic-babel-spinner))
      (should (eq mode-line-process nil)))))

