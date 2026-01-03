
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



(defclass toplevel (classified) ())


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



(defcomponent gallery (double-bracket) "gallery")


(defcomponent size (double-bracket classified) "size")
(defcomponent code (double-bracket classified) "code")
(defcomponent collapsible (double-bracket classified) "collapsible")
(defcomponent note (double-bracket classified) "note")
(defcomponent html (double-bracket classified) "html")
(defcomponent span (double-bracket classified) "span")
(defcomponent div (double-bracket classified) "div")
(defcomponent div_ (double-bracket classified) "div_" "/div")
(defcomponent math (double-bracket classified) "math")
(defcomponent footnote (double-bracket classified) "footnote")
(defcomponent module (double-bracket classified) "module")
(defcomponent iftags (double-bracket classified) "iftags")
(defcomponent tabview (double-bracket classified) "tabview")
(defcomponent tab (double-bracket classified) "tab")
(defcomponent bibliography (double-bracket classified) "bibliography")
(defcomponent a (double-bracket classified) "a")
(defcomponent a_ (double-bracket classified) "a_" "/a")
(defcomponent = (double-bracket classified) "=")
(defcomponent > (double-bracket classified) ">")
(defcomponent < (double-bracket classified) "<")
(defcomponent ul (double-bracket classified) "ul")
(defcomponent li (double-bracket classified) "li")
(defcomponent table (double-bracket classified) "table")
(defcomponent row (double-bracket classified) "row")
(defcomponent cell (double-bracket classified) "cell")



(defcomponent toc (double-bracket unclassified) "toc")
(defcomponent f>toc (double-bracket unclassified) "f>toc")
(defcomponent f<toc (double-bracket unclassified) "f<toc")
(defcomponent image (double-bracket unclassified) "image")
(defcomponent =image (double-bracket unclassified) "=image")
(defcomponent <image (double-bracket unclassified) "<image")
(defcomponent >image (double-bracket unclassified) ">image")
(defcomponent f<image (double-bracket unclassified) "f<image")
(defcomponent f>image (double-bracket unclassified) "f>image")
(defcomponent eref (double-bracket unclassified) "eref")
(defcomponent footnoteblock (double-bracket unclassified) "footnoteblock")
(defcomponent include (double-bracket unclassified) "include")
(defcomponent date (double-bracket unclassified) "date")
(defcomponent file (double-bracket unclassified) "file")
(defcomponent user (double-bracket unclassified) "user")
(defcomponent *user (double-bracket unclassified) "*user")
(defcomponent social (double-bracket unclassified) "social")
(defcomponent button (double-bracket unclassified) "button")
(defcomponent expr (double-bracket unclassified) "#expr")
(defcomponent if (double-bracket unclassified) "#if")
(defcomponent hashtag (double-bracket unclassified) "#")

