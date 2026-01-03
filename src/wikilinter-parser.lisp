
(in-package #:wikilinter-parser)



;; --------------------------------------------------
;; get-tag-and-params
;; --------------------------------------------------


(defun get-string-block-and-rest (text open-str close-str)
  "任意の文字列 open-str と close-str で囲まれたブロックを抽出する"
  (let* ((o-quoted (ppcre:quote-meta-chars open-str))
         (c-quoted (ppcre:quote-meta-chars close-str))
         ;; 境界ガード用の文字（最初と最後の文字）
         (o-char (ppcre:quote-meta-chars
		  (string (char open-str 0))))
         (c-char (ppcre:quote-meta-chars
		  (string (char close-str (1- (length close-str))))))
         ;; パターン構築
         ;; 1. (?<!o)open(?!o)  : open-strの前後が同じ文字でない
         ;; 2. (.*?)             : 中身（最短一致）
         ;; 3. (?<!c)close(?!c) : close-strの前後が同じ文字でない
         (pattern (format nil "(?s)(?<!~A)~A(?!~A)(.*?)(?<!~A)~A(?!~A)"
                          o-char o-quoted o-char
                          c-char c-quoted c-char)))
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan pattern text)
      (if match-start
          (values (subseq text (aref reg-starts 0) (aref reg-ends 0))
                  (subseq text match-end))
          (values nil text)))))


(defun get-tag-and-params (text open-str close-str)
  "テキストを受け取り、tagとparams、それ以外を返す"
  (multiple-value-bind (content other)
      (get-string-block-and-rest text open-str close-str)
    (let ((pos (position #\Space content)))
      (if pos
          ;; スペースがある場合：その前後で分割
          (values (string-downcase (subseq content 0 pos))
                  (subseq content (1+ pos))
                  other)
          ;; スペースがない場合：全体がタグ、paramsは空
          (values (string-downcase content)
                  ""
                  other)))))




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



;; --------------------------------------------------
;; %destruct-block-content
;; --------------------------------------------------



(define-condition component-not-found (error)
  ((tag-name :initarg :tag-name :reader tag-name))
  (:report (lambda (c s)
	     (format s "ERROR: component-not-found
\"~a\"は辞書に存在しないタグです。"
		     (tag-name c)))))


(defmacro push-content-to-queue (queue-handler values)
  (alexandria:with-gensyms (component remainder)
    `(multiple-value-bind (,component ,remainder) ,values
       (when component
	 (funcall ,queue-handler :push ,component))
       (when (or  remainder (string= "" remainder))
	 (funcall ,queue-handler :push ,remainder)))))



(defun %destruct-block-content (tag params remainder
			    loc-list-handler queue-handler)
  (multiple-value-bind (component-name)
      (wikilinter-components:tag->component tag)

    (unless component-name
      (error 'component-not-found :tag-name tag))
    
    (let ((component
	    (make-instance component-name
			   :params params
			   :location (funcall loc-list-handler :get))))

      
      (push-content-to-queue queue-handler
       (%destruct-ftml-block component
			     loc-list-handler
			     remainder)))))



;; --------------------------------------------------
;; if-tag-end-name
;; --------------------------------------------------


(define-condition unmatch-tag-end-name  (error)
  ((tag-end-name :initarg :tag-end-name :reader tag-end-name)
   (component-name :initarg :component-name
		   :reader component-name)
   (location :initarg :location :reader tag-location))
  
  (:report (lambda (c s) (format s "ERROR: unmatch-tag-end-name
~a行目: \"~a\"は\"~a\"の閉じタグではありません。
~a"
			    (%get-loc-data (tag-location c)
					  :pos)
			    (tag-end-name c)
			    (component-name c)
			    (%get-loc-data (tag-location c)
					  :str)))))


(defun tag-end-p (tag)
  "閉じタグかどうかを確認する"
  (char= (char tag 0) #\/))


(defmacro if-tag-end-name ((tag component) then &optional else)
  (alexandria:with-gensyms (end-name)
    (alexandria:once-only (tag component)
      `(let ((,end-name
	       (wikilinter-components:component-end-name ,component)))
	 (cond
	   ((string= ,tag ,end-name)
	    ,then)
	   ((tag-end-p ,tag)
	    (error 'unmatch-tag-end-name
		   :tag-end-name ,tag
		   :component-name
		   (wikilinter-components:component-name ,component)
		   :location
		   (wikilinter-components:component-location ,component)))
	   (t
	    ,else))))))




;; --------------------------------------------------
;; %destruct-ftml-block
;; --------------------------------------------------



(defun %get-loc-data  (loc-data key)
  (when loc-data
	 (case key
	   (:str (caar loc-data))
	   (:pos (cdar loc-data)))))


(defgeneric %destruct-ftml-block (component loc-list-handler
				  &optional remainder))


(defmethod %destruct-ftml-block ((component wikilinter-components:toplevel)
				 loc-list-handler
				 &optional remainder)
  (declare (ignorable remainder))
  
  (loop
    
    :with queue-handler
      := (wikilinter-components:component-content-queue-handler component)
    :and loc-data := (funcall loc-list-handler :get)
    :with initial-string := (%get-loc-data loc-data :str)

    :initially (unless (starts-with-exactly-n-chars-p #\[ 2 initial-string)
		 (funcall queue-handler :push initial-string)
		 (funcall loc-list-handler :next))

    :for current-loc
      := (funcall loc-list-handler :get)
	:then (funcall loc-list-handler :next)
    :for content := (%get-loc-data current-loc :str)
    :while content
    :do (multiple-value-bind (tag params remainder)
	    (get-tag-and-params content "[[" "]]")
	  (%destruct-block-content
	   tag params remainder loc-list-handler queue-handler))))





(defmethod %destruct-ftml-block ((component wikilinter-components:classified)
				 loc-list-handler
				 &optional remainder)
  
  (loop
    :with queue-handler
      := (wikilinter-components:component-content-queue-handler component)

    ;; reminderが空文字列でない限りpushする
    :initially (unless (string= "" remainder)
		 (funcall queue-handler :push remainder))

    :for current-loc := (funcall loc-list-handler :next)
    :for content := (%get-loc-data current-loc :str)
    :while content
    :do (multiple-value-bind (tag params outer)
	    (get-tag-and-params content "[[" "]]")
	  (if-tag-end-name (tag component)
			   (return (values component outer))
			   (%destruct-block-content
			    tag params outer
			    loc-list-handler queue-handler)))))




(defmethod %destruct-ftml-block ((component wikilinter-components:unclassified)
				 loc-list-handler
				 &optional remainder)
  (values component remainder))



;; --------------------------------------------------
;; %make-location-list
;; --------------------------------------------------


(defun %count-lines (str)
  (if (equal str "")
      0
      (count #\Newline str)))


(defun %make-location-list (string-list)
  (loop :for s :in string-list
        :for count := 1 :then (+ count prev-lines)
	:for prev-lines := (%count-lines s)
        :collect (cons s count)))



;; --------------------------------------------------
;; %make-location-list-handler
;; --------------------------------------------------

(defun %make-location-list-handler (string)
  (let* ((loc-list (%make-location-list (parse-ftml-text string 2)))
	 (pointer loc-list))
    (lambda (_) (case _
		 (:next (pop pointer)
			pointer)
		 (:get pointer)))))

;; --------------------------------------------------
;; destruct-ftml-block
;; --------------------------------------------------


(defun parse-ftml-text (text n)
  "n重の角括弧の『開始位置』だけを見つけ、次の開始位置までを一つの塊として切り出す"
  (let* (;; 開始タグ [[ だけを探す
	 (pattern (format nil "(?<!\\[)\\[{~D}(?!\\[)" n))
         (offsets (ppcre:all-matches pattern text))
         (results nil))
    (if (null offsets)
        (list text)
        (progn
          ;; 最初の [[ より前にテキスト（地の文）があれば確保
          (when (> (first offsets) 0)
            (push  (subseq text 0 (first offsets))
		   results))
          
          ;; offsets に入っているのは [start-pos end-pos ...]
          ;; start-pos (偶数番目) だけを使って、次の start-pos までを切る
          (loop for (current-start nil next-start) on offsets by #'cddr
                do (push (subseq text current-start
				 (or next-start (length text)))
			 results))
          
          (nreverse results)))))


(defun destruct-ftml-block (string)
  (let* ((loc-list-handler (%make-location-list-handler string))
	 (toplevel (make-instance 'wikilinter-components:toplevel
				  :location (funcall loc-list-handler :get))))
    (%destruct-ftml-block toplevel loc-list-handler)))




;; ------------------------
;; temp-codes
;; ------------------------

(defparameter *test-string*
  (uiop:read-file-string "~/common-lisp/wikidot-linter/data/test.txt"))

;; (%make-location-list (parse-ftml-text
;; 			*test-string* 2))

;; (destruct-ftml-block *test-string*)

(defparameter *test-object*
  (make-instance 'wikilinter-components:toplevel
		 :location (%make-location-list
			    (parse-ftml-text *test-string* 2))))


;; (funcall (%make-location-list-handler (parse-ftml-text *test-string* 2))
;;  :next)

;; (funcall (time
;; 	  (wikilinter-components:component-content-queue-handler
;; 	   (destruct-ftml-block *test-string*))))

;; (length *test-string*)


(destruct-ftml-block 
  "[[table style=\"border: none; width: 100%;\"]]
[[row]]
[[cell]]
[[size 125%]]**アイテム番号:** SCP-3363-JP[[/size]]
[[/cell]]
[[cell style=\"text-align: right;\"]]
[[size 125%]]##990000|**Level 0/3363-JP**##[[/size]]
[[/cell]]
[[/row]]
[[row]]
[[cell]]
[[size 125%]]**オブジェクトクラス:** Shahar[[span class=\"fnnum\"]].[[/span]][[span class=\"fncon\"]]**補足情報:** 人類文明の存続は危機的状況ですが、事態への対処法は確立しています。[[/span]]
[[/cell]]
[[cell style=\"text-align: right;\"]]
[[size 125%]]##990000|**Unclassified**##[[/size]]
[[/cell]]
[[/row]]
[[/table]]")

;; (funcall (%make-location-list-handler "[[table style=\"border: none; width: 100%;\"]]
;; [[row]]
;; [[cell]]
;; [[size 125%]]**アイテム番号:** SCP-3363-JP[[/size]]
;; [[/cell]]
;; [[cell style=\"text-align: right;\"]]
;; [[size 125%]]##990000|**Level 0/3363-JP**##[[/size]]
;; [[/cell]]
;; [[/row]]
;; [[row]]
;; [[cell]]
;; [[size 125%]]**オブジェクトクラス:** Shahar[[span class=\"fnnum\"]].[[/span]][[span class=\"fncon\"]]**補足情報:** 人類文明の存続は危機的状況ですが、事態への対処法は確立しています。[[/span]]
;; [[/cell]]
;; [[cell style=\"text-align: right;\"]]
;; [[size 125%]]##990000|**Unclassified**##[[/size]]
;; [[/cell]]
;; [[/row]]
;; [[/table]]" ) :get)




;; (time (parse-ftml-text "[[table style=\"border: none; width: 100%;\"]]
;; [[row]]
;; [[cell]]
;; [[size 125%]]**アイテム番号:** SCP-3363-JP[[/size]]
;; [[/cell]]
;; [[cell style=\"text-align: right;\"]]
;; [[size 125%]]##990000|**Level 0/3363-JP**##[[/size]]
;; [[/cell]]
;; [[/row]]
;; [[row]]
;; [[cell]]
;; [[size 125%]]**オブジェクトクラス:** Shahar[[span class=\"fnnum\"]].[[/span]][[span class=\"fncon\"]]**補足情報:** 人類文明の存続は危機的状況ですが、事態への対処法は確立しています。[[/span]]
;; [[/cell]]
;; [[cell style=\"text-align: right;\"]]
;; [[size 125%]]##990000|**Unclassified**##[[/size]]
;; [[/cell]]
;; [[/row]]
;; [[/table]]" 2))
