
(in-package #:wdlinter-fifo-queue)

;; --------------------------------------------------
;; make-fifo-queue-handler
;; --------------------------------------------------


(defstruct (queue (:constructor %make-queue))
  head
  tail)

(defun make-queue (&aux (queue nil))
  (%make-queue :head queue :tail queue))

(defun push-queue (obj queue)
  (let ((content (list obj)))
    (if (queue-head queue)
	(setf (cdr (queue-tail queue)) content
	      (queue-tail queue) (cdr (queue-tail queue)))
	(setf (queue-head queue) content
	      (queue-tail queue) content))))

(defun pop-queue (queue)
  (cond ((null (queue-head queue)) nil)
	((= 1 (length (queue-head queue)))
	 (pop (queue-head queue))
	 (setf (queue-tail queue) (queue-head queue)))
	(t (pop (queue-head queue)))))

(defun print-queue-list (queue &optional stream)
  (print (queue-head queue) stream)
  queue)



