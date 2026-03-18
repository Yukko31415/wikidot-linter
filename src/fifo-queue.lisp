
(in-package #:wdlinter-fifo-queue)

;; --------------------------------------------------
;; make-fifo-queue-handler
;; --------------------------------------------------


;;
;; make-queue

(defstruct (queue (:constructor %make-queue))
  "fifo-queueを構成する構造体"
  head
  tail)

(defun make-queue (&aux (queue nil))
  "make-queue => queue"
  (%make-queue :head queue :tail queue))


;;
;; qush-queue

(defun push-queue (obj queue)
  "push-queue obj queue => obj
   obj = an object"
  (let ((content (list obj)))
    (if (queue-head queue)
	(setf (cdr (queue-tail queue)) content
	      (queue-tail queue) (cdr (queue-tail queue)))
	(setf (queue-head queue) content
	      (queue-tail queue) content)))
  obj)


;;
;; pop-queue

(defun pop-queue (queue)
  "pop-queue queue => car-of-queue-head"
  (cond ((null (queue-head queue)) nil)
	((= 1 (length (queue-head queue)))
	 (pop (queue-head queue))
	 (setf (queue-tail queue) (queue-head queue)))
	(t (pop (queue-head queue)))))


;;
;; print-queue-list

(defun print-queue-list (queue &optional stream)
  "print-queue-list queue &optional stream => queue"
  (print (queue-head queue) stream)
  queue)

;;
;; merge-queue


(defun %merge-queue (queue1 queue2)
  (bind (((:accessors (head1 queue-head) (tail1 queue-tail))
	  queue1)
	 ((:accessors (head2 queue-head) (tail2 queue-tail))
	  queue2))
    (setf head1 (append head1 head2)
       tail1 tail2
       head2 nil
       tail2 nil))
  queue1)

(defun merge-queue (&rest queues)
  "merge-queue {queue}^1 => car-of-queues"
  (reduce #'%merge-queue queues))

