;; -*- lexical-binding: t -*-
(require 'rustic-doc)
(require 'f)

(ert-deftest rustic-doc-setup-test ()
  (rustic-doc-setup nil t)
    ;;  The setup is async, so we can't continue immediately.
  (should (file-exists-p rustic-doc-convert-prog))
  (should (file-exists-p rustic-doc-lua-filter))
  (while (get-buffer (concat "*" "rustic-doc-std-conversion" "*"))
    (sleep-for 1))
  (should (file-exists-p "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/option"))
  (should (file-exists-p (f-join rustic-doc-save-loc "std" "option" "enum.Option.org"))))
