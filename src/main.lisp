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
  (let ((textdata (uiop:read-file-string (uiop:parse-native-namestring (read-line)))))
    (terpri)
    (time (wikilinter-parser:destruct-ftml-block textdata)))
  (loop
    :initially (format t "qで終了~%")
	       (finish-output)
    :do (when (string= "q" (read-line))
	  (loop-finish))
    :finally (format t "終了します. . .")))
