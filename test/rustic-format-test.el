;; -*- lexical-binding: t -*-
;; Before editing, eval (load-file "test-helper.el")

(ert-deftest rustic-test-format-buffer ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (buffer-read-only nil))
    (with-current-buffer buf
      (erase-buffer)
      (rustic-mode)
      (insert string)
      (backward-char 10)
      (let ((proc (rustic-format-start-process
                   'rustic-format-sentinel
                   :buffer (current-buffer)
                   :stdin (buffer-string))))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (should (string= (buffer-string) formatted-string))
      (should-not (= (point) (or (point-min) (point-max)))))
    (kill-buffer buf)))

(ert-deftest rustic-test-format-buffer-failure ()
  (let* ((string "fn main()      {}")
         (string-dummy "can't format this string")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (buffer-read-only nil))
    (ignore-error (kill-buffer rustic-format-buffer-name))
    (with-current-buffer buf
      (erase-buffer)
      (insert string)
      (fundamental-mode)
      ;; no rustic-mode buffer
      (should-error (rustic-format-buffer))
      (should-not (get-buffer rustic-format-buffer-name))
      (erase-buffer)
      (rustic-mode)
      (insert string-dummy)
      (let* ((proc (rustic-format-start-process
                    'rustic-format-sentinel
                    :buffer (current-buffer)
                    :stdin (buffer-string)))
             (buf (process-buffer proc)))
        (with-current-buffer buf
          ;; check if buffer has correct name and correct major mode
          (should (string= (buffer-name buf) rustic-format-buffer-name))
          (should (eq major-mode 'rustic-format-mode)))))
    (kill-buffer buf)))

(ert-deftest rustic-test-format-file ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (main (buffer-file-name buf)))
    (write-region string nil main nil 0)
    (let ((proc (rustic-format-start-process
                 'rustic-format-file-sentinel
                 :buffer buf
                 :files main)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))
    (with-temp-buffer
      (insert-file-contents main)
      (should (string= (buffer-string) formatted-string)))
    (should-error (rustic-format-start-process
                   'rustic-format-file-sentinel
                   :buffer "dummy"
                   :files "/tmp/nofile"))))

(ert-deftest rustic-test-format-file-with-tabs ()
  (let* ((string "fn main()      {()}")
         (formatted-string "fn main() {\n\t()\n}\n")
         (rustic-rustfmt-config-alist '((hard_tabs . t)))
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (main (buffer-file-name buf)))
    (with-current-buffer buf (write-file main))
    (write-region string nil main nil 0)
    (let ((proc (rustic-format-start-process
                 'rustic-format-file-sentinel
                 :buffer buf
                 :files main)))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))
    (with-temp-buffer
      (insert-file-contents main)
      (should (string= (buffer-string) formatted-string)))))

(ert-deftest rustic-test-format-file-old-syntax ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (main (buffer-file-name buf)))
    (write-region string nil main nil 0)
    (let ((proc (rustic-format-start-process
                 'rustic-format-file-sentinel
                 :buffer buf
                 :command `(,(rustic-rustfmt-bin) ,main))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))
    (with-temp-buffer
      (insert-file-contents main)
      (should (string= (buffer-string) formatted-string)))))

(ert-deftest rustic-test-format-multiple-files ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (f-one (expand-file-name "one.rs" ))
         (f-two (expand-file-name "two.rs" ))
         (buf (get-buffer-create "test")))
    (with-current-buffer buf (write-file f-one) (write-file f-two))
    (write-region string nil f-one nil 0)
    (write-region string nil f-two nil 0)
    (let ((proc (rustic-format-start-process
                 'rustic-format-file-sentinel
                 :buffer buf
                 :files (list f-one f-two))))
      (while (eq (process-status proc) 'run)
        (sit-for 0.1)))
    (with-temp-buffer
      (insert-file-contents f-one)
      (should (string= (buffer-string) formatted-string)))
    (with-temp-buffer
      (insert-file-contents f-two)
      (should (string= (buffer-string) formatted-string)))))

(ert-deftest rustic-test-format-buffer-before-save ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (buf (rustic-test-count-error-helper-new string))
         (default-directory (file-name-directory (buffer-file-name buf)))
         (file (progn (shell-command-to-string "touch test.rs")
                      (expand-file-name "test.rs")))
         (buffer-read-only nil))
    (let ((rustic-format-trigger 'on-save))
      (with-current-buffer buf
        (write-file file)
        (erase-buffer)
        (rustic-mode)
        (insert string)
        (backward-char 10)
        (save-buffer)
        (if-let ((proc (get-process rustic-format-process-name)))
            (while (eq (process-status proc) 'run)
              (sit-for 0.01)))
        (should (string= (buffer-string) formatted-string))
        (should-not (= (point) (or (point-min) (point-max))))))
    (let ((buf (get-buffer-create "test-save-no-format"))
          (file (progn (shell-command-to-string "touch test-no-format.rs")
                       (expand-file-name "test-no-format.rs")))
          (rustic-format-trigger nil))
      (with-current-buffer buf
        (write-file file)
        (erase-buffer)
        (rustic-mode)
        (insert string)
        (save-buffer)
        (should (string= (buffer-string) (concat string "\n")))))
    (kill-buffer buf)))

(ert-deftest rustic-test-cargo-format ()
  (let* ((string "fn main()      {}")
         (formatted-string "#![allow(non_snake_case)]\nfn main() {}\n")
         (buffer1 (rustic-test-count-error-helper-new string))
         (dir (file-name-directory (buffer-file-name buffer1))))
    (let* ((default-directory dir)
           (file1 (expand-file-name "main.rs"))
           (rustic-format-trigger nil))
      ;; run 'cargo fmt'
      (call-interactively 'rustic-cargo-fmt)
      (if-let ((proc (get-process rustic-format-process-name)))
          (rustic-test--wait-till-finished rustic-format-buffer-name))
      (with-current-buffer buffer1
        (should (string= (buffer-string) formatted-string))))
    (kill-buffer buffer1)))

;; `rustic-format-trigger' is set to 'on-compile
(ert-deftest rustic-test-trigger-format-on-compile ()
  (ignore-errors (kill-buffer (get-buffer rustic-compilation-buffer-name)))
  (let* ((buffer1 (get-buffer-create "b1"))
         (string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t))
         (compilation-read-command nil))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           (rustic-format-trigger 'on-compile))
      (with-current-buffer buffer1
        (insert string)
        (write-file file1))

      ;; run `rustic-compile'
      (if-let ((proc (call-interactively 'rustic-compile)))
          (rustic-test--wait-till-finished rustic-compilation-buffer-name))
      ;; #352
      (should (get-buffer rustic-compilation-buffer-name))
      (with-current-buffer buffer1
        (revert-buffer t t)
        (should (string= (buffer-string) formatted-string)))
      (kill-buffer buffer1))))

;; #352 -> test if compilation doesn't start when format failed
(ert-deftest rustic-test-trigger-format-on-compile-format-error ()
  (ignore-errors (kill-buffer (get-buffer rustic-compilation-buffer-name)))
  (let* ((buffer1 (get-buffer-create "b1"))
         (string "ffn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t))
         (compilation-read-command nil))
    (let* ((default-directory dir)
           (src (concat dir "/src"))
           (file1 (expand-file-name "main.rs" src))
           (rustic-format-trigger 'on-compile))
      (with-current-buffer buffer1
        (insert string)
        (write-file file1))

      ;; run `rustic-compile'
      (if-let ((proc (call-interactively 'rustic-compile)))
          (rustic-test--wait-till-finished rustic-compilation-buffer-name))
      (should-not (get-buffer rustic-compilation-buffer-name))
      (kill-buffer buffer1))))

(ert-deftest rustic-test-format-region ()
  (let* ((buffer1 (get-buffer-create "b1"))
         (string "fn main() {\n    let x =\n        1;\n}")
         (formatted-string "fn main() {\n    let x = 1;\n}\n")
         (dir (rustic-babel-generate-project t))
         (main (expand-file-name "main.rs" (concat dir "/src")))
         (default-directory dir))
    (with-current-buffer buffer1
      (insert string)
      (write-file main)
      (rustic-mode)
      (goto-char 16)
      (push-mark 34)
      (setq mark-active t)
      (rustic-format-region 16 34)
      (sit-for 1)
      (should (string= (buffer-string) formatted-string)))))

(ert-deftest rustic-test-format-args-and-config-args ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (rustic-rustfmt-config-alist '((hard_tabs . t) (skip_children . nil)))
         (rustic-rustfmt-args "+stable --edition 2021")
         (dir (rustic-babel-generate-project t))
         (main (expand-file-name "main.rs" (concat dir "/src")))
         (buf (get-buffer-create "test")))
    (with-current-buffer buf (write-file main))
    (write-region string nil main nil 0)
    (with-current-buffer buf
      (let ((proc (rustic-format-start-process
                   'rustic-format-file-sentinel
                   :buffer buf
                   :files main)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1))))
    (with-temp-buffer
      (insert-file-contents main)
      (should (string= (buffer-string) formatted-string)))))


;; rustfmt.toml

(defun rustic-test-format-create-rustfmt-toml (dir contents)
  (let ((rustfmt-toml (expand-file-name "rustfmt.toml" dir)))
    (with-current-buffer (get-buffer-create "rustfmt-toml")
      (write-file rustfmt-toml))
    (write-region contents nil rustfmt-toml nil 0)))

(ert-deftest rustic-test-rustfmt-toml-file ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t))
         (main (expand-file-name "main.rs" (concat dir "/src")))
         (inhibit-read-only t)
         (buf (get-buffer-create "test")))
    (rustic-test-format-create-rustfmt-toml dir "foo = 1\nbar = 2")
    (write-region string nil main nil 0)

    (with-current-buffer buf
      (insert string)
      (rustic-mode)
      (write-file main))
    (with-current-buffer buf
      (let ((proc (rustic-format-buffer)))
        (while (eq (process-status proc) 'run)
          (sit-for 0.1)))
      (should (string= (buffer-string) formatted-string)))
    (kill-buffer buf)))

(ert-deftest rustic-test-rustfmt-toml-file-format-file-with-warning ()
  (let* ((string "fn main()      {}")
         (formatted-string "fn main() {}\n")
         (dir (rustic-babel-generate-project t))
         (main (expand-file-name "main.rs" (concat dir "/src")))
         (buf (get-buffer-create "test")))
    (rustic-test-format-create-rustfmt-toml dir "foo = 1\nbar = 2")
    (write-region string nil main nil 0)

    (with-current-buffer buf (write-file main))
    (write-region string nil main nil 0)
    (with-current-buffer buf
     (let ((proc (rustic-format-start-process
                  'rustic-format-file-sentinel
                  :buffer buf
                  :files main)))
       (while (eq (process-status proc) 'run)
         (sit-for 0.1))))
    (with-temp-buffer
      (insert-file-contents main)
      (should (string= (buffer-string) formatted-string)))))


