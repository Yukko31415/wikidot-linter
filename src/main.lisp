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
      (prog1 (uiop:read-file-string (uiop:parse-native-namestring (read-line)))
	(terpri))
    (sb-ext:file-does-not-exist ()
      (format t "ファイルが見つかりませんでした。もう一度入力してください。~%")
      (finish-output)
      (get-string-from-file))))

(defun get-string-and-destruct-ftml ()
  (let ((textdata (get-string-from-file)))
    (time (wikilinter-parser:destruct-ftml-block textdata))
    (format t "qで終了/nで次のファイル~%")
    (finish-output)))


(defun main ()
  "Entry point for the application."
  (loop
    :initially (get-string-and-destruct-ftml)

    :for command := (read-line)

    :until (string= "q" command)

    :when (string= "n" command)
      :do (get-string-and-destruct-ftml)

    :finally (format t "終了します...")))

