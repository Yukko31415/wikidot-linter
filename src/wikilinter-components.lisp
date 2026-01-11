
(in-package #:wikilinter-components)



(defparameter *component-classes* (make-hash-table :test #'equal))


(defclass component ()
  ((name :reader component-name)
   (bracketcount :reader component-bracketcount)
   (classp :reader component-classp)
   (params
    :initarg :params
    :reader component-params)
   (location
    :initarg :location
    :reader component-location)))

(defclass single-bracket (component)
  ((bracketcount :initform 1)))

(defclass double-bracket (component)
  ((bracketcount :initform 2)))

(defclass triple-bracket (component)
  ((bracketcount :initform 3)))

(defclass classified (component)
  ((classp :initform t)
   (end-name :reader component-end-name)
   (content-queue-handler
    :initform (wikilinter-fifo-queue:make-fifo-queue-handler)
    :reader component-content-queue-handler)))

(defclass unclassified (component)
  ((classp :initform nil)))



(defclass toplevel (classified)
  ((name :initform nil)
   (end-name :initform nil)))


(defun tag->component (tagname)
  (gethash (string-downcase tagname) *component-classes*))


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
      (push `(end-name :initform ,(cl:if end-name-p
					 end-name
					 (format nil "/~A" component-name)))
	    slots))
    
    `(progn
       (defclass ,class-name ,direct-superclasses ,slots)
       (setf (gethash ,component-name *component-classes*) ',class-name))))




(defmacro component-list (direct-superclasses list)
  "defcomponentに展開する"
  (let ((defcomponents
	  (loop :for  (name component-name &optional (end-name nil end-name-p)) in list 
		:collect (cl:if end-name-p
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

