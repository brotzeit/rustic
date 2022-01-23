;; -*- lexical-binding: t -*-
(require 'rustic-doc)

(ert-deftest create-dir-test ()
  (make-directory "~/rustic-doc-test"))

(ert-deftest rustic-doc-setup-test ()
  (rustic-doc-setup nil t)
  (should (file-exists-p rustic-doc-convert-prog))
  (should (file-exists-p rustic-doc-lua-filter))
  (print (format "rust option html: %s" (directory-files "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/option")))
  (print (directory-files (concat  rustic-doc-save-loc "/std")))
  ;; (print  (directory-files (concat rustic-doc-save-loc "/std/option/")))
  (print "conversion log:")
  (print (with-current-buffer "*rustic-doc-std-conversion*"
           (print (buffer-string))))
  (should (file-exists-p (concat rustic-doc-save-loc "/std/option/enum.Option.org"))))
