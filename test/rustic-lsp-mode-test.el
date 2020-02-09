;; -*- lexical-binding: t -*-

(setq lsp-restart 'ignore)

;; TODO: this shouldn't be necessary
(require 'lsp-mode)
(require 'lsp-rust)

(ert-deftest rustic-test-rustic-lsp-server ()
  "Test if value of `rustic-lsp-server' is used and priority set correctly."
  (let ((rustic-lsp-setup-p t)
        (rustic-lsp-server 'rust-analyzer))
    ;; check if rust-analyzer starts
    (let* ((dir (rustic-babel-generate-project t))
           (main (expand-file-name "main.rs" (concat dir "/src")))
           (buf (get-buffer-create "test")))
      (with-current-buffer buf
        (write-file main))
      (with-current-buffer buf
        (sit-for 1)
        (should (lsp-find-workspace 'rust-analyzer default-directory))
        (with-lsp-workspace (lsp-find-workspace 'rust-analyzer default-directory)
          (lsp--shutdown-workspace))
        (sit-for 1)
        (should-not (lsp-find-workspace 'rust-analyzer default-directory)))
      (kill-buffer buf))))


