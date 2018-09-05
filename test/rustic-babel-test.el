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
      (forward-line -1))
    buf))

(defun rustic-test-babel-wait ()
  "Wait for babel results."
  (sit-for 2)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+RESULTS:\n: cargo" nil t)
      (goto-char (point-min))
      (sit-for 0.5))))

(defun rustic-test-babel-execute-block (buf)
  "Execute babel block in BUF."
  (with-current-buffer buf
    (call-interactively 'org-ctrl-c-ctrl-c)
    (rustic-test-babel-wait)))

(defun rustic-test-check-results (string buf)
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
    (rustic-babel-test-execute-block buf)
    (should (rustic-babel-check-results "foo\n" buf))
    (should-not (buffer-live-p rustic-babel-compilation-buffer-name))))
