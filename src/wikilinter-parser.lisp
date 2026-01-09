
(in-package #:wikilinter-parser)



;; --------------------------------------------------
;; get-tag-and-params
;; --------------------------------------------------



(defun %get-string-block-and-rest (text)
  (declare (type simple-string text))
  (let ((scanner (load-time-value
		  (ppcre:create-scanner
		   "(?s)(?<!\\[)\\[\\[(?!\\[)(.*?)(?<!\\])\\]\\](?!\\])"))))
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
	(ppcre:scan scanner text) ;; 文字列ではなくコンパイル済みスキャナを渡す
      (if match-start
	  (values (subseq text (aref reg-starts 0) (aref reg-ends 0))
		  (subseq text match-end))
	  (values nil text)))))


(defun get-tag-and-params (text)
  "テキストを受け取り、tagとparams、それ以外を返す。nilが入力された場合はnilを返す。
タグとパラメータの区切りとしてスペース、改行、タブを許容する。"
  (multiple-value-bind (content other)
      (if text (%get-string-block-and-rest text) (values nil nil))
    (if (null content)
	(values nil nil other)		; contentがnilの場合
	(let ((pos (position-if (lambda (c)
				  (member c '(#\Space #\Newline #\Tab #\Return)))
				content)))
	  (if pos
	      ;; 区切り文字が見つかった場合
	      (values (string-downcase (subseq content 0 pos))
		      ;; paramsの先頭の空白もついでにトリム
		      (string-left-trim '(#\Space #\Newline #\Tab #\Return)
					(subseq content (1+ pos)))
		      other)
	      ;; 区切り文字がない場合
	      (values (string-downcase content)
		      ""
		      other))))))




;; --------------------------------------------------
;; utils
;; --------------------------------------------------


(defun starts-with-exactly-n-chars-p (char n string)
  "文字列 string の先頭が、ちょうど n 個の char で始まっているか判定する"
  (let ((len (length string)))
    (and (>= len n)		 ; まず長さが n 以上あるか
       ;; 0からn番目までがすべて char か確認
       (loop for i from 0 below n
             always (char= (char string i) char))
       ;; n番目の文字が char ではない（あるいは文字列がそこで終わっている）ことを確認
       (or (= len n)
	  (not (char= (char string n) char))))))


(defun get-loc-data  (loc-data key)
  (when loc-data
    (case key
      (:str (caar loc-data))
      (:pos (cdar loc-data)))))



;; --------------------------------------------------
;; %destruct-block-content
;; --------------------------------------------------



(define-condition component-not-found (error)
  ((tag-name :initarg :tag-name :reader tag-name)
   (location :initarg :location :reader location))
  (:report (lambda (c s)
	     (format s "ERROR: component-not-found
~A行目: \"~a\"は辞書に存在しないタグです。"
		     (location c)
		     (tag-name c)))))


(defmacro %push-content-to-queue (queue-handler values)
  (alexandria:with-gensyms (component remainder)
    `(multiple-value-bind (,component ,remainder) ,values
       ;; コンポーネントが存在する場合にpush
       (when component
	 (funcall ,queue-handler :push ,component))
       ;; remainderの長さが0あるいはnilでない場合にpush
       (unless (or (zerop (length ,remainder)) (null ,remainder))
	 (funcall ,queue-handler :push ,remainder)))))



(defun %destruct-block-content (queue-handler loc-list-handler tag params remainder)
  (multiple-value-bind (component-name)
      (wikilinter-components:tag->component tag)
    (unless component-name
      (error 'component-not-found
	     :tag-name tag
	     :location (get-loc-data (funcall loc-list-handler :get)
				     :pos)))
    (let ((component
	    (make-instance component-name
			   :params params
			   :location (funcall loc-list-handler :get))))
      (%push-content-to-queue
       queue-handler
       (%destruct-ftml-block component loc-list-handler remainder)))))


;; --------------------------------------------------
;; if-tag-end-name
;; --------------------------------------------------


(define-condition unmatch-tag-end-name (error)
  ((tag-end-name :initarg :tag-end-name :reader tag-end-name)
   (current-pos :initarg :current-pos :reader current-pos)
   (location :initarg :component-location :reader component-location)
   (component-name :initarg :component-name :reader component-name))

  (:report (lambda (c s) (format s "ERROR: unmatch-tag-end-name
~a行目: \"~a\"は、
~a行目: \"~a\"の閉じタグではありません。
~a"
			    (current-pos c)
			    (tag-end-name c)
			    (get-loc-data (component-location c)
					  :pos)
			    (component-name c)
			    (get-loc-data (component-location c)
					  :str)))))


(defun %tag-end-p (tag)
  "閉じタグかどうかを確認する"
  (char= (char tag 0) #\/))


(defmacro if-tag-end-name (((tag current-pos) component) then &optional else)
  (alexandria:with-gensyms (end-name)
    (alexandria:once-only (tag component current-pos)
      `(let ((,end-name (wikilinter-components:component-end-name
			 ,component)))
	 (cond ((string= ,tag ,end-name)
		,then)
	       ((%tag-end-p ,tag)
		(error
		 'unmatch-tag-end-name
		 :tag-end-name ,tag
		 :component-name (wikilinter-components:component-name
				  ,component)
		 :component-location (wikilinter-components:component-location
				      ,component)
		 :current-pos ,current-pos))
	       (t ,else))))))


;; --------------------------------------------------
;; handle-unmatch-tag-end-name
;; --------------------------------------------------


(defgeneric handle-unmatch-tag-end-name (component condition))


(defmethod handle-unmatch-tag-end-name ((component wikilinter-components:toplevel) condition)
  "unmatch-tag-end-nameがトップレベルまで解決されなかった場合、そのタグは無効な閉じタグとして処理されるべきである"
  (let ((current-pos (current-pos condition))
	(tag-end-name (tag-end-name condition)))
    (progn
      (format t "~A行目: \"~A\"は無効な閉じタグです~%~%"
	      current-pos tag-end-name)
      (invoke-restart 'ignore-tag))))


(defmethod handle-unmatch-tag-end-name ((component wikilinter-components:classified) condition)
  "classifiedコンポーネント内でunmatch-tag-end-nameが解決された場合、その子の閉じタグが存在しないと考えることができる"
  (let ((component-end-name (wikilinter-components:component-end-name component))
	(tag-end-name (tag-end-name condition))
	(component-name (component-name condition))
	(loc (get-loc-data (component-location condition) :pos))
	(str (get-loc-data (component-location condition) :str)))
    (when (string= tag-end-name component-end-name)
      (progn
	(format t "~A行目: \"~A\"の閉じタグが見つかりません~%~A~%~%"
		loc component-name str)
	(invoke-restart 'close-tag)))))



;; --------------------------------------------------
;; %destruct-ftml-block
;; --------------------------------------------------


(defgeneric %%destruct-ftml-block (component loc-list-handler &optional remainder))




(defmethod %%destruct-ftml-block ((component wikilinter-components:toplevel)
				 loc-list-handler &optional remainder)
  (declare (ignorable remainder))
  (loop
    :with queue-handler
      := (wikilinter-components:component-content-queue-handler component)
    :and loc-data := (funcall loc-list-handler :get)
    :with initial-string := (get-loc-data loc-data :str)

    :initially (unless (starts-with-exactly-n-chars-p #\[ 2 initial-string)
		 (funcall queue-handler :push initial-string)
		 (funcall loc-list-handler :next))

    :for current-loc := (funcall loc-list-handler :get)
    :for content := (get-loc-data current-loc :str)
    :do (multiple-value-bind (tag params remainder) (get-tag-and-params content)
	  (restart-case
	      (if-tag-end-name ((tag (get-loc-data current-loc :pos)) component)
			       (return component)
			       (%destruct-block-content queue-handler loc-list-handler tag params remainder))
	    (ignore-tag ()
	      :report "タグを無視して続行する"
	      (progn
		(funcall queue-handler :push content)
		(funcall loc-list-handler :next)))))))



(defmethod %%destruct-ftml-block ((component wikilinter-components:classified)
				 loc-list-handler &optional remainder)
  (loop
    :with queue-handler
      := (wikilinter-components:component-content-queue-handler component)

    ;; reminderが空文字列でない限りpushする
    :initially (funcall loc-list-handler :next)
	       (unless (zerop (length remainder))
		 (funcall queue-handler :push remainder))

    :for current-loc := (funcall loc-list-handler :get)
    :for content := (get-loc-data current-loc :str)
    
    :do (multiple-value-bind (tag params outer) (get-tag-and-params content)
	  (restart-case
	      (if-tag-end-name ((tag (get-loc-data current-loc :pos)) component)
			       (progn
				 (funcall loc-list-handler :next)
				 (return (values component outer)))
			       (%destruct-block-content queue-handler loc-list-handler tag params outer))
	    (close-tag ()
	      :report "タグを閉じて続行する"
	      (return (values component "")))
	    (ignore-tag ()
	      :report "タグを無視して続行する"
	      (progn
		(funcall queue-handler :push content)
		(funcall loc-list-handler :next)))))))


(defmethod %%destruct-ftml-block ((component wikilinter-components:unclassified)
				  loc-list-handler &optional remainder)
  (progn
    (funcall loc-list-handler :next)
    (values component remainder)))




(defun %destruct-ftml-block (component loc-list-handler &optional remainder)
  (handler-bind
      ((unmatch-tag-end-name
	 #'(lambda (c) (handle-unmatch-tag-end-name component c)))
       (component-not-found
	 #'(lambda (c) (format t "~A行目: \"~a\"は辞書に存在しないタグです。" (location c) (tag-name c))
	     (invoke-restart 'ignore-tag))))
    (%%destruct-ftml-block component loc-list-handler remainder)))



;; --------------------------------------------------
;; make-location-list
;; --------------------------------------------------


(defun %count-lines (str)
  (declare (type simple-string str))
  (if (equal str "")
      0
      (count #\Newline str)))


(defun make-location-list (string-list)
  (loop :for s :in string-list
        :for count := 1 :then (+ count prev-lines)
	:for prev-lines := (%count-lines s)
        :collect (cons s count)))



;; --------------------------------------------------
;; make-location-list-handler
;; --------------------------------------------------



(defun %parse-ftml-text (text)
  "n重の角括弧の『開始位置』だけを見つけ、次の開始位置までを一つの塊として切り出す"
  (let* (;; 開始タグ [[ だけを探す
	 (pattern (load-time-value
		   (ppcre:create-scanner
		    "(?<!\\[)\\[{2}(?!\\[)")))
	 (offsets (ppcre:all-matches pattern text))
	 (results nil))
    (if (null offsets)
	(list text)
	(progn
	  ;; 最初の [[ より前にテキスト（地の文）があれば確保
	  (when (> (first offsets) 0)
	    (push (subseq text 0 (first offsets))
		  results))

	  ;; offsets に入っているのは [start-pos end-pos ...]
	  ;; start-pos (偶数番目) だけを使って、次の start-pos までを切る
	  (loop :for (current-start nil next-start)
		  :on offsets :by #'cddr
		:do (push (subseq text current-start
				  (or next-start (length text)))
			  results))

	  (nreverse results)))))

(defun make-location-list-handler (string)
  (let* ((loc-list (make-location-list (%parse-ftml-text string)))
	 (pointer loc-list))
    (lambda (_) (case _
	     (:next (pop pointer))
	     (:get pointer)))))




;; --------------------------------------------------
;; destruct-ftml-block
;; --------------------------------------------------


(defun destruct-ftml-block (string)
  (declare (type simple-string string))
  (let* ((loc-list-handler (make-location-list-handler string))
	 (toplevel (make-instance 'wikilinter-components:toplevel
				  :location (funcall loc-list-handler :get))))
    (%destruct-ftml-block toplevel loc-list-handler)))




;; ------------------------
;; temp-codes
;; ------------------------

(defparameter *test-string*
  (uiop:read-file-string "~/common-lisp/wikidot-linter/data/test.txt"))

(defparameter *test-object*
  (make-instance 'wikilinter-components:toplevel
		 :location (make-location-list
			    (%parse-ftml-text *test-string*))))

;; (make-location-list (%parse-ftml-text *test-string*))



;; (time (destruct-ftml-block *test-string*))












