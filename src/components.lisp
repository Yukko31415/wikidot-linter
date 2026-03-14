
(in-package #:wdlinter-components)



(defparameter *component-classes* (make-hash-table :test #'equal))


(defclass toplevel ()
  ((content-queue
    :initform (wdlinter-fifo-queue:make-queue)
    :reader component-content-queue)))

(defclass component ()
  ((name :reader component-name)
   (bracketcount :reader component-bracketcount)
   (classp :reader component-classp)
   (params
    :initarg :params
    :reader component-params)))


;;
;; bracket


(defclass single-bracket (component)
  ((bracketcount :initform 1)))

(defclass double-bracket (component)
  ((bracketcount :initform 2)))

(defclass triple-bracket (component)
  ((bracketcount :initform 3)))


;;
;; classfied, unclassfied


(defclass classified (component)
  ((classp :initform t)
   (end-name :reader component-end-name)
   (content-queue
    :initform (wdlinter-fifo-queue:make-queue)
    :reader component-content-queue)))

(defclass unclassified (component)
  ((classp :initform nil)))


;;
;; util


(define-condition unknown-component (error)
  ((tagname :initarg :tagname :reader tagname))
  (:report (lambda (c s) (format s "\"~A\"は存在しないコンポーネントです"
			    (tagname c)))))

(defun tag->component (tagname)
  (when tagname
    (multiple-value-bind (component find)
	(gethash (string-downcase tagname) *component-classes*)
      (common-lisp:if find component
		      (error 'unknown-component :tagname tagname)))))

(defun end-name= (component tagname)
  (string= tagname (component-end-name component)))

(defun end-tag-p (tagname)
  (alexandria:starts-with #\/ tagname))

(defun push-content (obj component)
  (wdlinter-fifo-queue:push-queue
   obj (component-content-queue component)))


;; --------------------------
;; components
;; --------------------------

(defmacro defcomponent (class-name direct-superclasses component-name
		 &optional (end-name nil end-name-p))
  (let ((slots `((name :initform ,component-name))))
    ;; スーパークラスのリストに 'classified が含まれている場合のみ、
    ;; end-name のスロット定義をリストに追加する
    ;; end-nameが特殊に指定されている場合はそれを用いる
    (when (member 'classified direct-superclasses)
      (push `(end-name :initform ,(common-lisp:if end-name-p
					 end-name
					 (format nil "/~A" component-name)))
	    slots))
    `(progn
       (defclass ,class-name ,direct-superclasses ,slots)
       (setf (gethash ,component-name *component-classes*) ',class-name))))

(defmacro component-list (direct-superclasses list)
  "defcomponentに展開する"
  (let ((defcomponents
	  (loop :for (name component-name end-name) :in list
		:collect (common-lisp:if end-name
				`(defcomponent ,name ,direct-superclasses
				     ,component-name ,end-name)
				`(defcomponent ,name ,direct-superclasses
				     ,component-name)))))
    `(progn ,@defcomponents)))






(defcomponent gallery (double-bracket) "gallery")


(component-list
 (double-bracket classified)
 ((size "size")
  (code "code")
  (collapsible "collapsible")
  (note "note")
  (html "html")
  (span "span")
  (div "div")
  (div_ "div_" "/div")
  (math "math")
  (footnote "footnote")
  (module "module")
  (iftags "iftags")
  (tabview "tabview")
  (tab "tab")
  (bibliography "bibliography")
  (a "a")
  (a_ "a_" "/a")
  (= "=")
  (> ">")
  (< "<")
  (ul "ul")
  (li "li")
  (table "table")
  (row "row")
  (cell "cell")))


(component-list
 (double-bracket unclassified)
 ((toc "toc")
  (f>toc "f>toc")
  (f<toc "f<toc")
  (image "image")
  (=image "=image")
  (<image "<image")
  (>image ">image")
  (f<image "f<image")
  (f>image "f>image")
  (eref "eref")
  (footnoteblock "footnoteblock")
  (include "include")
  (date "date")
  (file "file")
  (user "user")
  (*user "*user")
  (social "social")
  (button "button")
  (expr "#expr")
  (if "#if")
  (hashtag "#")))

