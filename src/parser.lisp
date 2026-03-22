
(in-package #:wdlinter-parser)




;;
;; ftml-ref


(defun duplicate-string (original &key (start 0) (end (length original)))
  (make-array (- end start) :element-type 'character
			    :displaced-to original
			    :displaced-index-offset start))

(defun print-ftml-ref (obj stream)
  (let ((string (duplicate-string (ftml-ref-string obj)
				  :start (ftml-ref-start obj)
				  :end (ftml-ref-end obj))))
    (format stream "\"~A\"" string)))

(defstruct (ftml-ref (:print-object print-ftml-refA))
  string
  start
  end)





;; --------------------------------------------------
;; parse-ftml-text
;; --------------------------------------------------

(defstruct (parsed-ftml (:constructor %make-parsed-ftml) (:conc-name nil))
  ftml-string
  ftml-line
  ftml-location
  ftml-length)

(defun make-loc-list (text)
  ;; 二重角括弧の開始点をのリストを渡す
  (let* ((pattern (load-time-value (ppcre:create-scanner "(?<!\\[)\\[{2}(?!\\[)")))
	 (offsets (ppcre:all-matches pattern text))
	 (result (make-array (1+ (ceiling (length offsets) 2))
			     :fill-pointer 0)))
    (when offsets (loop :for i :in offsets :by #'cddr
			:do (vector-push i result)
			:count i :into length
			:finally (vector-push (length text) result)
				 (return (values result length))))))

(defun %count-lines (str start end)
  (declare (type simple-string str))
  (count #\Newline str :start start :end end))

(defun count-lines (str loc &aux (line 0) (start 0))
  (flet ((fn (end) (prog1 (incf line (%count-lines str start end))
		     (setf start end))))
    (map 'vector #'fn loc)))

(defun parse-ftml-text (string)
  (bind (((:values loc length) (make-loc-list string)))
    (%make-parsed-ftml :ftml-string string
		       :ftml-location loc
		       :ftml-line (count-lines string loc)
		       :ftml-length length)))





;;
;; ftml-refs


(declaim (ftype (function (integer parsed-ftml) (values integer integer))
		ftml-ref-line ftml-ref-loc))

(defun ftml-ref-loc (index parsed-ftml)
  (with-slots (ftml-location ftml-length) parsed-ftml
    (when (> ftml-length index)
      (let* ((start (aref ftml-location index))
	     (end (aref ftml-location (1+ index))))
	(values start end)))))

(defun ftml-ref-line (index parsed-ftml)
  (with-slots (ftml-length ftml-line) parsed-ftml
    (when (> ftml-length index)
      (values (aref ftml-line index)
	      (aref ftml-line (1+ index))))))

(defun ftml-ref (index parsed-ftml)
  (with-slots (ftml-location ftml-length ftml-string) parsed-ftml
    (when (> ftml-length index)
      (let* ((start (aref ftml-location index))
	     (end (aref ftml-location (1+ index))))
	(make-ftml-ref :string ftml-string :start start :end end)))))

(defun ftml-header (parsed-ftml)
  (with-slots (ftml-location ftml-string) parsed-ftml
    (unless (zerop (aref ftml-location 0))
      (let ((end (aref ftml-location 0)))
	(make-ftml-ref :string ftml-string :start 0 :end end)))))




;; --------------------------------------------------
;; get-tag-and-params
;; --------------------------------------------------

(defun get-block-range-of (text &key (start 0) (end (length text)))
  ;; コードブロックとそれ以外を分ける
  (bind ((scanner (load-time-value
		   (ppcre:create-scanner "(?s)(?<!\\[)\\[\\[(?!\\[).*?(?<!\\])\\]\\](?!\\])")))
	 ((:values match-start match-end) (ppcre:scan scanner text :start start :end end)))
    (values match-start match-end)))

(defun get-all-whitespace-blocs (text &key (start 0) (end (length text)))
  (let ((scanner (load-time-value (ppcre:create-scanner "[ \\n\\t\\r]+"))))
    (ppcre:all-matches scanner text :start start :end end)))

(defun get-block-range-of/for-parsed-ftml (index parsed-ftml)
  (bind (((:values start end) (ftml-ref-loc index parsed-ftml))
	 ((:values match-start match-end)
	  (get-block-range-of (ftml-string parsed-ftml) :start start :end end)))
    (values start end match-start match-end)))


(defun get-tag-and-params (index parsed-ftml)
  ;; get-tag-and-params index parsed-ftml => tag, params, other
  ;; index = a non negative integer.
  ;; tag = a ftml-ref object.
  ;; params = a list of ftml-ref objects.
  ;; other = a ftml-ref object.
  (bind (((:slots ftml-string) parsed-ftml)
	 ((:values _ end match-start match-end)
	  (get-block-range-of/for-parsed-ftml index parsed-ftml))
	 ((&optional tag &rest loc-list)
	  (get-all-whitespace-blocs (ftml-string parsed-ftml) :start match-start :end match-end)))

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



;; --------------------------------------------------
;; destruct-ftml-block/toplevel
;; --------------------------------------------------


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
    (t (component:push-content obj component))))

(defun push-contents (component &rest args)
  ;; contentがqueueである場合、componentとマージし、
  ;; そうでない場合、componentのcontent-queueにpushする
  (mapc #'(lambda (cont) (when cont (%push-contents component cont)))
	args)
  component)


;;
;; utils


(defun end-tag-p/for-ftml-ref (ftml-ref)
  (with-slots (ftml-ref-string ftml-ref-start) ftml-ref
    (component:end-tag-p
     ftml-ref-string :start ftml-ref-start)))

(defun end-name=/for-ftml-ref (component ftml-ref)
  (with-slots (ftml-ref-string ftml-ref-start ftml-ref-end) ftml-ref
    (component:end-name= component ftml-ref-string
			 :start ftml-ref-start
			 :end ftml-ref-end)))




;;
;; %destruct-ftml-block


(defun %destruct-ftml-block (parsed-ftml counter)
  (multiple-value-bind (tagname params other) (get-tag-and-params counter parsed-ftml)
    (if (end-tag-p/for-ftml-ref tagname) (values tagname other counter)
	(multiple-value-bind (content outer counter)
	    (%%destruct-ftml-block (make-instance (component:tag->component tagname)
						  :params params)
				   other parsed-ftml counter)
	  (values content outer counter)))))

(defmacro %destruct-ftml-block/on-toplevel (parsed-ftml counter)
  `(handler-bind
       ((unmatch-tag-end-name
	  #'(lambda (c)
	      (restart-case (error 'invalid-end-tag-name :end-name (end-name c)
							 :crr-line (crr-line c))
		(ignore () :report "タグを無視して続行します。"
		  (invoke-restart 'ignore))))))
     (%destruct-ftml-block ,parsed-ftml ,counter)))

(defmacro %destruct-ftml-block/with-handler (component parsed-ftml counter)
  `(handler-bind
       ((unmatch-tag-end-name
	  #'(lambda (c) (when (end-name=/for-ftml-ref ,component (end-name c))
			  (signal 'parse-time-log :condition c)
			  (invoke-restart 'close-and-rewind)))))
     (%destruct-ftml-block ,parsed-ftml ,counter)))





;;
;; %%destruct-ftml-block


(defgeneric %%destruct-ftml-block (component other parsed-ftml crr-loc))

(defmethod %%destruct-ftml-block ((component component:classified)
				  other parsed-ftml crr-loc)
  (unless (string= "" other) (component:push-content other component))
  (loop :for counter := (1+ crr-loc) :then (1+ counter)
	:do (multiple-value-bind (content outer crr-loc)
		(%destruct-ftml-block/with-handler component parsed-ftml counter)
	      (etypecase content
		(component:component (push-contents component content outer))
		(string (if (end-name=/for-ftml-ref component content)
			    (return (values component outer crr-loc))
			    (restart-case (error 'unmatch-tag-end-name
						 :component component
						 :end-name content
						 :crr-line (ftml-ref-line crr-loc parsed-ftml))
			      (close-and-rewind () (return (values component nil (1- crr-loc))))
			      (ignore () (push-contents component content outer))))))
	      (setf counter crr-loc))))

(defmethod %%destruct-ftml-block ((component component:unclassified)
				  other parsed-ftml crr-loc)
  (values component other crr-loc))




;;
;; destruct-ftml-block/toplevel


(defun destruct-ftml-block/toplevel (parsed-ftml)
  (let ((toplevel (make-instance 'component:toplevel))
	(header (ftml-header parsed-ftml)))
    (when header (component:push-content header toplevel))
    (loop :for counter := 0 :then (1+ counter)
	  :while (> (ftml-length parsed-ftml) counter)
	  :do (multiple-value-bind (content outer crr-loc)
		  (%destruct-ftml-block/on-toplevel parsed-ftml counter)
		(push-contents toplevel content outer)
		(setf counter crr-loc)))
    toplevel))






;; --------------------------------------------------
;; destruct-ftml-block
;; --------------------------------------------------


(defun destruct-ftml-block (string &key (stream t) &aux (parsed-ftml (parse-ftml-text string))
						 (log nil))
  "ftmlブロックを作成する。エラーを出力し、toplevelコンポーネントを返す"
  (declare (type simple-string string))
  (handler-bind ((parse-time-log #'(lambda (c) (push (log-condition c) log)))
		 (invalid-end-tag-name #'(lambda (c) (push c log) (invoke-restart 'ignore))))
    (prog1 (destruct-ftml-block/toplevel parsed-ftml)
      (format stream "~{~A~%~}" (sort log #'(lambda (a b) (< (crr-line a) (crr-line b)))))
      (terpri))))



