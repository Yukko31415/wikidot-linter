;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Yukko


(in-package #:wikidot-linter)




(defun get-string-from-file ()
  (format t "ファイルパスを入力: ")
  (finish-output)
  (handler-case
      (uiop:read-file-string (uiop:parse-native-namestring (read-line)))
    (sb-ext:file-does-not-exist ()
      (format t "ファイルが見つかりませんでした。もう一度入力してください。~%")
      (finish-output)
      (get-string-from-file))))



(defun main ()
  "Entry point for the application."
  (loop
    :for textdata := (get-string-from-file)

    :do (terpri)
	(time (wikilinter-parser:destruct-ftml-block textdata))
	(format t "qで終了/nで次のファイル~%")
	(finish-output)
	(alexandria:switch ((read-line) :test #'string=)
	  ("q" (loop-finish))
	  ("n" nil))
    :finally (format t "終了します. . .")))

