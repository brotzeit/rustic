;; -*- lexical-binding: t -*-

(require 'f)

(let ((rustic-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path rustic-dir))

(require 'rustic)
