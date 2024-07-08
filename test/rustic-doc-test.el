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
(ert-deftest rustic-doc-semver-test ()
  (should (equal (rustic-doc--semver-from-string "1.0.0") '(1 0 0)))
  (should (equal (rustic-doc--semver-from-string "1.0.0-alpha") '(1 0 0)))
  (should (equal (rustic-doc--semver-from-string "1.0.0.1") '(1 0 0)))
  
  (setq cmp-test-vectors (list (list "1.0.0" "1.0.0.1" nil)
                               (list "1.0.0" "1.0.0-alpha" nil)
                               (list "2.0.1" "2.0.0" t)
                               (list "2.1.0" "2.0.0" t)
                               (list "2.1.1" "2.1.0" t)))

  (dolist (test cmp-test-vectors)
    (let ((v1 (rustic-doc--semver-from-string (car test)))
          (v2 (rustic-doc--semver-from-string (cadr test)))
          (expected (caddr test)))
      (should (equal (rustic-doc--semver-greater v1 v2) expected)))))

(ert-deftest rustic-doc-verstion-extract-test ()
  (should (equal (rustic-doc--extract-version "fd 10.1.0
") (list 10 1 0)))
  (should (equal (rustic-doc--extract-version "pandoc 3.1.11.1
Features: +server +lua
Scripting engine: Lua 5.4
User data directory: /Users/user/.local/share/pandoc
Copyright (C) 2006-2023 John MacFarlane. Web: https://pandoc.org
This is free software; see the source for copying conditions. There is no
warranty, not even for merchantability or fitness for a particular purpose.") (list 3 1 11))))
