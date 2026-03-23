;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Yukko


(in-package #:wikidot-linter)




(defun get-string-from-file ()
  (loop (format t "ファイルパスを入力: ") (finish-output)
	(handler-bind ((error
			 (lambda (c) (declare (ignore c)) (invoke-restart 'retry))))
	  (restart-case
	      (return (prog1 (uiop:read-file-string (uiop:parse-native-namestring (read-line)))
			(terpri)))
	    (retry () (format t "ファイルが見つかりませんでした。もう一度入力してください。~%")
	      (finish-output))))))

(defun get-string-and-destruct-ftml ()
  (let ((textdata (get-string-from-file)))
    (time (wdlinter-parser:parse-ftml textdata))
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

   :finally (format t "終了します...") (uiop:quit)))


