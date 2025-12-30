
(in-package #:wikilinter-parser)




(defgeneric %handle-fifo-queue (key arg arg-p head tail)
  (:documentation "fifo-queueを扱うためのハンドル関数"))

(defun %push-fifo-queue (arg head tail)
  "fifoキューに値をプッシュする関数"
  (let ((new (list arg)))
    (if (consp head)
	(progn (setf (cdr tail) new
		     tail new)
	       (values head head tail))
	(progn (setf head new
		     tail head)
	       (values head head tail)))))

(defmethod %handle-fifo-queue ((key (eql :push)) arg arg-p  head tail)
  (declare (ignorable arg-p))
  (if arg-p
      (%push-fifo-queue arg head tail)
      (values head head tail)))

(defmethod %handle-fifo-queue ((key (eql :pop)) arg arg-p  head tail)
  (declare (ignorable arg-p))
  (if (consp head)
      (values (pop head) head tail)
      (values nil head head)))

(defmethod %handle-fifo-queue ((key (eql :view)) arg arg-p head tail)
  (declare (ignorable arg-p))
  (values head head tail))

(defun make-fifo-queue ()
  (let* ((head (list))
	 (tail head))
    (lambda (&optional (key :view) (arg nil arg-p))
      (multiple-value-bind (value new-head new-tail)
	  (%handle-fifo-queue key arg arg-p head tail)
	(setf head new-head
	      tail new-tail)
	value))))




(defgeneric %destruct-ftml-block (component string-list))

(defmethod % ((component wikilinter-components:toplevel))
  )



(defun get-tag (text)
  (car (uiop:split-string text
			  :separator '(#\ ))))

(defun split-by-opening-bracket (text)
  (cl-ppcre:split "\\["
		  text))


(defun block-end-p (text)
  "blockを閉じるものか判定するための関数"
  (string= #\/ (subseq text 0 1)))


(defun destruct-ftml-block (string)
  (let ((toplevel (make-instance 'wikilinter-components:toplevel
				 :content (make-fifo-queue)))
	(string-list
	  (split-by-opening-bracket string)))
    (%destruct-ftml-block toplevel
			  string-list)))


(defun parse-ftml-text (text)
  (cl-ppcre:do-register-groups (content)
      ("\\[\\[(.*?)\\]\\]" text)))






;; ------------------------
;; temp-codes
;; ------------------------



(defparameter *test-string*
  (uiop:read-file-string "~/common-lisp/wikidot-linter/data/test.txt"))







