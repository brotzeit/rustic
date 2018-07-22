;; -*- lexical-binding: t -*-

(require 'f)
(require 'ert)
(require 'cl)
(require 'imenu)

(let ((rustic-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path rustic-dir))

(require 'rustic)
