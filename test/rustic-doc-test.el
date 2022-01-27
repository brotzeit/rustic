;; -*- lexical-binding: t -*-
(require 'rustic-doc)
(require 'f)
(ert-deftest create-dir-test ()
  (make-directory "~/rustic-doc-test"))

(ert-deftest rustic-doc-setup-test ()
  (rustic-doc-setup nil t)
  (while (get-buffer (concat "*" "rustic-doc-std-conversion" "*"))
    ;;  The setup is async, so we can't continue immediately.
    (sleep-for 1))
  (should (file-exists-p rustic-doc-convert-prog))
  (should (file-exists-p rustic-doc-lua-filter))
  (should (file-exists-p "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/option"))
  (should (file-exists-p (f-join rustic-doc-save-loc "std" "option" "enum.Option.org"))))
