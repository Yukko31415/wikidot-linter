
(in-package #:wikilinter-fifo-queue)

;; --------------------------------------------------
;; make-fifo-queue-handler
;; --------------------------------------------------


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

(defmethod %handle-fifo-queue ((key (eql :push)) arg arg-p head tail)
  (declare (ignorable arg-p))
  (if (and arg-p arg)
      (%push-fifo-queue arg head tail)
      (values head head tail)))

(defmethod %handle-fifo-queue ((key (eql :pop)) arg arg-p head tail)
  (declare (ignorable arg-p))
  (cond ((eq head tail) (values (pop head) head head))
	((consp head) (values (pop head) head tail))
	(t (values nil head head))))

(defmethod %handle-fifo-queue ((key (eql :view)) arg arg-p head tail)
  (declare (ignorable arg-p))
  (values head head tail))

(defun make-fifo-queue-handler ()
  (let* ((head (list))
	 (tail head))
    (lambda (&optional (key :view) (arg nil arg-p))
      (multiple-value-bind (value new-head new-tail)
	  (%handle-fifo-queue key arg arg-p head tail)
	(setf head new-head
	      tail new-tail)
	value))))



