;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name

(in-package #:wikidot-linter)

(defun main ()
  "Entry point for the application."
  (format t "ファイルパスを入力: ")
  (finish-output)
  (wikilinter-parser:destruct-ftml-block
   (uiop:read-file-string (uiop:parse-native-namestring (read-line)))))
