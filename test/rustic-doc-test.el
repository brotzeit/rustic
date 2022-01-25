;; -*- lexical-binding: t -*-
(require 'rustic-doc)

(ert-deftest create-dir-test ()
  (make-directory "~/rustic-doc-test"))

(ert-deftest rustic-doc-setup-test ()
  (rustic-doc-setup nil t)
  (should (file-exists-p rustic-doc-convert-prog))
  (should (file-exists-p rustic-doc-lua-filter))
  (should (file-exists-p "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/option"))
  (print (format "rustic document location: %s" (concat rustic-doc-save-loc "/std/option/")))
  (print (format "files in std option: %s" (directory-files (concat rustic-doc-save-loc "/std/option/"))))
  (should (file-exists-p (concat rustic-doc-save-loc "/std/option/enum.Option.org"))))
