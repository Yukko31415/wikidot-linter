
(in-package #:wikilinter-parser)


;; --------------------------------------------------
;; destruct-ftml-block
;; --------------------------------------------------


(defgeneric %destruct-ftml-block (component string-list-handler
				   &optional remainder))

(defmacro push-content-to-queue (queue-handler values)
  `(multiple-value-bind (component remainder) ,values
     (when component
       (funcall ,queue-handler :push component))
     (when remainder
       (funcall ,queue-handler :push remainder))))


(defun %destruct-block-content (key params remainder
			    queue-handler string-list-handler)
  (multiple-value-bind (component-name)
      (wikilinter-components:tag->component key)
    (let ((component
	    (make-instance component-name
			   :params params)))

      (push-content-to-queue queue-handler
			     (%destruct-ftml-block component
						   string-list-handler
						   remainder)))))


(defmethod %destruct-ftml-block ((component wikilinter-components:toplevel)
				 string-list-handler &optional remainder)
  (declare (ignorable remainder))
  (let ((queue-handler (wikilinter-components:component-content component))
	(initial-string (funcall string-list-handler :view)))

    ;; 先頭が非コンポーネントで始まっていた場合、キューにpushする
    (when (starts-with-exactly-n-chars-p #\[ 2 initial-string)
      (funcall queue-handler :push initial-string)
      (setf initial-string (funcall string-list-handler :next)))

    (loop
      #:for content = initial-string
	#:then (funcall string-list-handler :next)
      #:do
	(if content
	    (multiple-value-bind (key params remainder)
		(get-tag-and-params content)
	      (%destruct-block-content
	       key params remainder
	       queue-handler string-list-handler))
	    (return (values component remainder))))))


(defmethod %destruct-ftml-block ((component wikilinter-components:classified)
				 string-list-handler &optional remainder)
  (let ((queue-handler (wikilinter-components:component-content component)))
    (funcall queue-handler :push remainder)
    (loop
      #:do
	(multiple-value-bind (key params outer)
	    (get-tag-and-params (funcall string-list-handler :next))
	  (if (string= key
		       (wikilinter-components:component-end-name component))
	      (return (values component outer))
	      (%destruct-block-content
	       key params outer
	       queue-handler string-list-handler))))))


(defmethod %destruct-ftml-block ((component wikilinter-components:unclassified)
				 string-list-handler &optional remainder)
  (values component remainder))






(defun get-bracket-and-rest (text)
  "テキストを受け取り、角括弧で囲まれた部分とそうでない部分を返す" 
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (ppcre:scan "\\[+([^\\[\\]]+)\\]+" text)
    (declare (ignorable match-start))
    (values (subseq text (aref reg-starts 0) (aref reg-ends 0))
	    (subseq text match-end))))

(defun get-tag-and-params (text)
  "テキストを受け取り、tagとparams、それ以外を返す"
  (multiple-value-bind (content other) (get-bracket-and-rest text)
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

(defun parse-ftml-text (text n)
  "n重の角括弧から始まる文字列のみを抽出する (1 <= n <= 4)"
  (let ((pattern (format nil "(?<!\\[)\\[{~D}(?!\\[)([^\\[]+)" n)))
    (ppcre:all-matches-as-strings pattern text)))


(defun starts-with-exactly-n-chars-p (char n string)
  "文字列 string の先頭が、ちょうど n 個の char で始まっているか判定する"
  (let ((len (length string)))
    (and (>= len n) ; まず長さが n 以上あるか
         ;; 0からn番目までがすべて char か確認
         (loop for i from 0 below n
               always (char= (char string i) char))
         ;; n番目の文字が char ではない（あるいは文字列がそこで終わっている）ことを確認
         (or (= len n)
             (not (char= (char string n) char))))))

(defun make-string-list-handler (string-list)
  (let ((list string-list))
    (lambda (_) (case _
	     (:view (car list))
	     (:next
	      (setf list (cdr list))
	      (car list))))))


(defun destruct-ftml-block (string)
  (let ((toplevel (make-instance 'wikilinter-components:toplevel))
	(string-list-handler
	  (make-string-list-handler (parse-ftml-text string 2))))
    (%destruct-ftml-block toplevel
			  string-list-handler)))




;; ------------------------
;; temp-codes
;; ------------------------

;; (defparameter *test-string*
;;   (uiop:read-file-string "~/common-lisp/wikidot-linter/data/test.txt"))

;; (funcall (make-string-list-handler (parse-ftml-text *test-string* 2))
;;  :next)

(funcall (time (wikilinter-components:component-content
		(destruct-ftml-block *test-string*))))




