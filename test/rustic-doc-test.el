;; -*- lexical-binding: t -*-
(require 'rustic-doc)

(ert-deftest rustic-doc-setup-test ()
  (rustic-doc-setup nil t)
  (should (file-exists-p rustic-doc-convert-prog))
  (should (file-exists-p rustic-doc-lua-filter))
  (print "echoing directory")
  (print (directory-files rustic-doc-save-loc))
  (print  (directory-files (concat rustic-doc-save-loc "/std/option/")))
  (should (file-exists-p (concat rustic-doc-save-loc "/std/option/enum.Option.org"))))
