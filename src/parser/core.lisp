
(in-package #:wdlinter-parser)



;; ------------
;;;; ftml-ref
;; ------------


(defstruct (ftml-ref (:print-object print-ftml-ref))
  string
  start
  end)

(defun duplicate-string (original &key (start 0) (end (length original)))
  (make-array (- end start) :element-type 'character
			    :displaced-to original
			    :displaced-index-offset start))

(defun print-ftml-ref (obj stream)
  (bind (((:slots string start end) obj)
	 (string (duplicate-string string :start start :end end)))
    (if *print-escape*
	(format stream "\"~A\"" string)
	(format stream "~A" string))))





;; -------------------
;;;; make-indexed-ftml
;; -------------------


(defstruct (indexed-ftml (:constructor %make-indexed-ftml) (:conc-name nil)
			(:print-object print-indexed-ftml)) 
  ftml-string
  ftml-line
  ftml-location
  ftml-length)

(defun print-indexed-ftml (obj stream)
  (print-unreadable-object (obj stream)
    (format stream "indexed-ftml length: ~A" (ftml-length obj))))

(defun make-loc-list (text)
  ;; 二重角括弧の開始点をのリストを渡す
  (let* ((pattern (load-time-value (ppcre:create-scanner "(?<!\\[)\\[{2}(?!\\[)")))
	 (offsets (ppcre:all-matches pattern text))
	 (result (make-array (1+ (ceiling (length offsets) 2)) :element-type 'fixnum)))
    (when offsets (loop :for ofs :in offsets :by #'cddr
			:and index :from 0
			:do (setf (aref result index) ofs)
			:count ofs :into length
			:finally (setf (aref result (1+ index)) (length text))
				 (return (values result length))))))

(defun %count-lines (str start end)
  (declare (type simple-string str))
  (count #\Newline str :start start :end end))

(defun count-lines (str loc &aux (line 0) (start 0))
  (flet ((fn (end) (prog1 (incf line (%count-lines str start end))
		     (setf start end))))
    (map 'vector #'fn loc)))

(defun make-indexed-ftml (string)
  (bind (((:values loc length) (make-loc-list string)))
    (%make-indexed-ftml :ftml-string string
			:ftml-location loc
			:ftml-line (count-lines string loc)
			:ftml-length length)))



;;
;; ftml-refs



(-> (ftml-ref-line ftml-ref-loc) (integer indexed-ftml) (values integer integer))

(defun ftml-ref-loc (index indexed-ftml)
  (with-slots (ftml-location ftml-length) indexed-ftml
    (when (> ftml-length index)
      (let* ((start (aref ftml-location index))
	     (end (aref ftml-location (1+ index))))
	(values start end)))))

(defun ftml-ref-line (index indexed-ftml)
  (with-slots (ftml-length ftml-line) indexed-ftml
    (when (> ftml-length index)
      (values (aref ftml-line index)
	      (aref ftml-line (1+ index))))))

(defun ftml-ref (index indexed-ftml)
  (with-slots (ftml-location ftml-length ftml-string) indexed-ftml
    (when (> ftml-length index)
      (let* ((start (aref ftml-location index))
	     (end (aref ftml-location (1+ index))))
	(make-ftml-ref :string ftml-string :start start :end end)))))

(defun ftml-header (indexed-ftml)
  ;; indexed-ftmlの先頭に角括弧以外のテキストがある場合、それを返す。
  ;; 存在しない場合はnilを返す。
  (with-slots (ftml-location ftml-string) indexed-ftml
    (unless (zerop (aref ftml-location 0))
      (let ((end (aref ftml-location 0)))
	(make-ftml-ref :string ftml-string :start 0 :end end)))))




;; ----------------------
;;;; get-tag-and-params
;; ----------------------

(defun get-block-range-of (text &key (start 0) (end (length text)))
  ;; コードブロックとそれ以外を分ける
  (bind ((scanner (load-time-value
		   (ppcre:create-scanner "(?s)(?<!\\[)\\[\\[(?!\\[).*?(?<!\\])\\]\\](?!\\])")))
	 ((:values match-start match-end) (ppcre:scan scanner text :start start :end end)))
    (values match-start match-end)))

(defun get-all-whitespace-blocs (text &key (start 0) (end (length text)))
  (let ((scanner (load-time-value (ppcre:create-scanner "[ \\n\\t\\r]+"))))
    (ppcre:all-matches scanner text :start start :end end)))

(defun get-block-range-of/for-indexed-ftml (index indexed-ftml)
  (bind (((:values start end) (ftml-ref-loc index indexed-ftml))
	 ((:values match-start match-end)
	  (get-block-range-of (ftml-string indexed-ftml) :start start :end end)))
    (values start end match-start match-end)))


(defun get-tag-and-params (index indexed-ftml)
  ;; get-tag-and-params index indexed-ftml => tag, params, other
  ;; index = a non negative integer.
  ;; tag = a ftml-ref object.
  ;; params = a list of ftml-ref objects.
  ;; other = a ftml-ref object.
  (bind (((:slots ftml-string) indexed-ftml)
	 ((:values _ end match-start match-end)
	  (get-block-range-of/for-indexed-ftml index indexed-ftml))
	 ((&optional tag &rest loc-list)
	  (get-all-whitespace-blocs (ftml-string indexed-ftml) :start match-start :end match-end)))

    (values (make-ftml-ref :string ftml-string :start (+ 2 match-start)
			   :end (or tag (- match-end 2)))
	    (loop :for (start end) :on loc-list :by #'cddr
	      :if end :collect (make-ftml-ref :string ftml-string :start start :end end)
		:else :if (> (- match-end 2) start)
			:collect (make-ftml-ref :string ftml-string :start start
						:end (- match-end 2))
	      :end
	      :while end)
	    (make-ftml-ref :string ftml-string :start match-end :end end))))



;; --------------------------------
;;;; make-ftml-component/toplevel
;; --------------------------------


;;
;; errors


(define-condition parse-time-log (condition)
  ((condition :initarg :condition :reader log-condition)))

(define-condition ftml-parse-time-error (error)
  ((crr-line :initarg :crr-line :reader crr-line)))

(define-condition unmatch-tag-end-name (ftml-parse-time-error)
  ((component :initarg :component :reader component)
   (end-name :initarg :end-name :reader end-name))
  (:report (lambda (c s) (format s "~A行目: [[~A]]は[[~A]]の閉じタグではありません"
				 (crr-line c) (end-name c) (component:component-name (component c))))))

(define-condition invalid-end-tag-name (ftml-parse-time-error)
  ((end-name :initarg :end-name :reader end-name))
  (:report (lambda (c s) (format s "~A行目: [[~A]]は無効なタグです"
				 (crr-line c) (end-name c)))))



;;
;; push-contents


(defun %push-contents (component obj)
  (typecase obj
    (fifo-queue:queue (component:merge-content-queue component obj))
    (ftml-ref (unless (= (ftml-ref-start obj) (ftml-ref-end obj))
		(component:push-content  obj component)))
    (t (component:push-content  obj component))))

(defun push-contents (component &rest args)
  ;; contentがqueueである場合、componentとマージし、
  ;; そうでない場合、componentのcontent-queueにpushする
  (mapc #'(lambda (cont) (when cont (%push-contents component cont)))
	args)
  component)


;;
;; utils

(defun end-tag-p/for-ftml-ref (ftml-ref)
  (with-slots (string start) ftml-ref
    (component:end-tag-p string start)))

(defun end-name=/for-ftml-ref (component ftml-ref)
  (with-slots (string start end) ftml-ref
    (component:end-name= component string :start start :end end)))

(defun tag->component/for-ftml-ref (ftml-ref)
  (with-slots (string start end) ftml-ref
    (component:tag->component string :start start :end end)))




;;
;; make-ftml-component


(defun make-ftml-component (indexed-ftml counter)
  (multiple-value-bind (tagname params other) (get-tag-and-params counter indexed-ftml)
    (if (end-tag-p/for-ftml-ref tagname) (values tagname other counter)
	(multiple-value-bind (content outer counter)
	    (%make-ftml-component (make-instance (tag->component/for-ftml-ref tagname)
						  :params params)
				   other indexed-ftml counter)
	  (values content outer counter)))))

(defmacro make-ftml-component/on-toplevel (indexed-ftml counter)
  `(handler-bind
       ((unmatch-tag-end-name
	  #'(lambda (c)
	      (restart-case (error 'invalid-end-tag-name :end-name (end-name c)
							 :crr-line (crr-line c))
		(ignore () :report "タグを無視して続行します。"
		  (invoke-restart 'ignore))))))
     (make-ftml-component ,indexed-ftml ,counter)))

(defmacro make-ftml-component/with-handler (component indexed-ftml counter)
  `(handler-bind
       ((unmatch-tag-end-name
	  #'(lambda (c) (when (end-name=/for-ftml-ref ,component (end-name c))
			  (signal 'parse-time-log :condition c)
			  (invoke-restart 'close-and-rewind)))))
     (make-ftml-component ,indexed-ftml ,counter)))





;;
;; %make-ftml-component


(defgeneric %make-ftml-component (component other indexed-ftml crr-loc))

(defmethod %make-ftml-component ((component component:classified)
				  other indexed-ftml crr-loc)
  (loop :initially (push-contents component other)
	:for counter :from (1+ crr-loc)
	:do (multiple-value-bind (content outer crr-loc)
		(make-ftml-component/with-handler component indexed-ftml counter)
	      (etypecase content
		(component:component (push-contents component content outer))
		(ftml-ref (if (end-name=/for-ftml-ref component content)
			      (return (values component outer crr-loc))
			      (restart-case (error 'unmatch-tag-end-name
						   :component component
						   :end-name content
						   :crr-line (ftml-ref-line crr-loc indexed-ftml))
				(close-and-rewind () (return (values component nil (1- crr-loc))))
				(ignore () (push-contents component content outer))))))
	      (setf counter crr-loc))))

(defmethod %make-ftml-component ((component component:unclassified)
				  other indexed-ftml crr-loc)
  (values component other crr-loc))

(defmethod %make-ftml-component ((component component:double-bracket)
				  other indexed-ftml crr-loc)
  (values component other crr-loc))




;;
;; make-ftml-component/toplevel


(defun make-ftml-component/toplevel (indexed-ftml)
  (let ((toplevel (make-instance 'component:toplevel))
	(header (ftml-header indexed-ftml)))
    (when header (push-contents  toplevel header))
    (loop :for counter :from 0 :below (ftml-length indexed-ftml)
	  :do (multiple-value-bind (content outer crr-loc)
		  (make-ftml-component/on-toplevel indexed-ftml counter)
		(push-contents toplevel content outer)
		(setf counter crr-loc)))
    toplevel))




;; -----------------------
;;;; make-ftml-component
;; -----------------------


(defun parse-ftml (string &key (stream nil) &aux (indexed-ftml (make-indexed-ftml string))
						 (log nil))
  "parse-ftml string &key stream => component-toplevel"
  (declare (type simple-string string))
  (handler-bind ((parse-time-log #'(lambda (c) (push (log-condition c) log)))
		 (invalid-end-tag-name #'(lambda (c) (push c log) (invoke-restart 'ignore))))
    (prog1 (make-ftml-component/toplevel indexed-ftml)
      (format stream "~{~A~%~}" (sort log #'(lambda (a b) (< (crr-line a) (crr-line b)))))
      (terpri))))





