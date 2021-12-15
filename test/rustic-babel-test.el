;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

(setq org-confirm-babel-evaluate nil)

(defun rustic-test-get-babel-block (contents &optional params)
  "Return buffer containing babel block with CONTENTS."
  (let ((buf (get-buffer-create "babel-test")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert (format "#+BEGIN_SRC rustic %s\n" (or params "")))
      (insert contents)
      (insert "\n#+END_SRC")
      buf)))

(defun rustic-test-babel-wait ()
  "Wait for babel results."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+RESULTS:\n: cargo" nil t)
      (goto-char (point-min))
      (sit-for 0.1))))

(defun rustic-test-babel-execute-block (buf &optional nowait)
  "Execute babel block in BUF."
  (with-current-buffer buf
    (rustic-test-silence
     '("executing %s code block%s..."
       "Code block evaluation complete.")
     (call-interactively 'org-ctrl-c-ctrl-c)
     (unless nowait
       (rustic-test-babel-wait)))))

(defun rustic-test-babel-check-results (buf)
  "Return babel result block contents in BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (search-forward "#+RESULTS:\n:\s" nil t)
      (goto-char (match-end 0))
      (buffer-substring-no-properties (point) (point-max)))))

(ert-deftest rustic-test-babel-result ()
  (let* ((string "fn main() {
                    println!(\"{}\", \"foo\");
                  }")
         (buf (rustic-test-get-babel-block string)))
    (rustic-test-babel-execute-block buf)
    (should (string= (rustic-test-babel-check-results buf) "foo\n"))
    (should-not (buffer-live-p rustic-babel-compilation-buffer-name)))
  (let* ((string "fn main() {
                    let _foo = 1;
                  }")
         (buf (rustic-test-get-babel-block string)))
    (rustic-test-babel-execute-block buf)
    (should (eq (rustic-test-babel-check-results buf) nil))
    (should-not (buffer-live-p rustic-babel-compilation-buffer-name))))

(ert-deftest rustic-test-babel-error-results ()
  (let* ((string "fn main() {
                     let v = vec![1, 2, 3];
                     b[1];
                   }")
         (buf (rustic-test-get-babel-block string)))
    (rustic-test-babel-execute-block buf)
    (let ((re (format "error: Could not compile `%s`.\n"
                      (car (reverse (split-string rustic-babel-dir "/"))))))
      (should (string= re (rustic-test-babel-check-results buf)))))
  (let* ((string "fn main() {
                     let b = vec![1, 2, 3];
                     b[99];
                   }")
         (buf (rustic-test-get-babel-block string)))
    (rustic-test-babel-execute-block buf)
    (let ((re "^thread '[^']+' panicked at '[^']+', "))
      (should (string-match re (rustic-test-babel-check-results buf))))))

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
        (sit-for 0.1))))
  ;; check if spinner stops in case of build error
  (let* ((string "fn main() {")
         (buf (rustic-test-get-babel-block string)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should-not (spinner-p rustic-babel-spinner))
      (should (eq mode-line-process nil))))
  ;; check if spinner stops in case of thread panic
  (let* ((string " fn main() {
                     let v = vec![1, 2, 3];
                     v[99];
                 }")
         (buf (rustic-test-get-babel-block string)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should-not (spinner-p rustic-babel-spinner))
      (should (eq mode-line-process nil)))))

(ert-deftest rustic-test-babel-format ()
  (let* ((string "fn main()      {}")
         (formatted-string "  fn main() {}\n")
         (buf (rustic-test-get-babel-block string)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (cl-loop with result
               repeat 3
               until (setq result
                           (string=
                            (org-element-property :value (org-element-at-point))
                            formatted-string))
               do (sleep-for 0 1000)
               do (message (buffer-string))
               finally (should result))))
  ;; turn off rustic-babel-format-src-block
  (let* ((string "fn main()      {}")
         (newstring (concat string "\n"))
         (buf (rustic-test-get-babel-block string))
         (rustic-babel-format-src-block nil))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (cl-loop with result
               repeat 3
               until (setq result
                           (string=
                            (org-element-property :value (org-element-at-point))
                            newstring))
               do (sleep-for 0 1000)
               finally (should result)))))

(ert-deftest rustic-test-babel-crate ()
  (let* ((string "extern crate rand;
                  fn main() {
                      let _rng = rand::thread_rng();
                  }")
         (params ":crates '((rand . 0.4))")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))

(ert-deftest rustic-test-babel-crate-without-version ()
  (let* ((string "extern crate rand;
                  extern crate serde;
                  extern crate serde_json;
                  fn main() {
                      let _rng = rand::thread_rng();
                  }")
         ;; also test with strings for crate and version
         (params ":crates '(rand (serde . *) (\"serde_json\" . \"*\"))")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))

(ert-deftest rustic-test-babel-crate-with-single-feature ()
  (let* ((string "extern crate serde;
                  extern crate serde_json;
                  #[derive(serde::Serialize)]
                  pub struct Point {
                      x: u32,
                      y: u32,
                  }
                  fn main() {
                      let point = Point {x: 0, y: 0};
                      let _json = serde_json::to_string(&point);
                  }")
         (params ":crates '((serde . 1.0) (serde_json . 1.0)) :features '((serde . \"derive\"))")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))

(ert-deftest rustic-test-babel-crate-with-multiple-features ()
  (let* ((string "extern crate tokio;
                  fn main() {
                      tokio::runtime::Runtime::new()
                          .unwrap()
                          .block_on(async {
                              tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                          });
                  }")
         (params ":crates '((tokio . 1.0)) :features '((tokio . (\"rt-multi-thread\" \"time\")))")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))

(ert-deftest rustic-test-babel-ensure-main-wrap-no()
  (let* ((string "let x = \"fn main(){}\";")
         (params ":main no")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (let ((re (format "error: Could not compile `%s`.\n"
                        (car (reverse (split-string rustic-babel-dir "/"))))))
        (should (string= re (rustic-test-babel-check-results buf)))))))

(ert-deftest rustic-test-babel-ensure-main-wrap-yes-with-main()
  (let* ((string "fn main() {
                      let x = \"rustic\";
                 }")
         (params ":main yes")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))

(ert-deftest rustic-test-babel-ensure--main-wrap-yes-with-async-main()
  (let* ((string "#[tokio::main]
                  pub async fn main() {
                      ()
                 }")
         (params ":crates '((tokio . 1.0)) :features '((tokio . (\"rt-multi-thread\" \"macros\"))) :main yes")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))

(ert-deftest rustic-test-babel-ensure-main-wrap-no-with-main()
  (let* ((string "fn main() {
                      let x = \"rustic\";
                  }")
         (params ":main no")
         (buf (rustic-test-get-babel-block string params)))
    (with-current-buffer buf
      (rustic-test-babel-execute-block buf)
      (should (eq (rustic-test-babel-check-results buf) nil)))))
