
(in-package #:wikilinter-components)


(defparameter *component-classes* (make-hash-table :test #'equal))

(defclass component ()
  ((name :reader component-name)
   (bracketcount :reader component-bracketcount)
   (classp :reader component-classp)
   (params
    :initarg :params
    :reader component-params)
   (content
    :initarg :content
    :reader component-content)
   (raw
    :initarg :raw
    :reader component-raw)))

(defclass single-bracket (component)
  ((bracketcount :initform 1)))

(defclass double-bracket (component)
  ((bracketcount :initform 2)))

(defclass triple-bracket (component)
  ((bracketcount :initform 3)))

(defclass classified (component)
  ((classp :initform t)))

(defclass unclassified (component)
  ((classp :initform nil)))



(defclass toplevel (component))


;; --------------------------
;; components
;; --------------------------

(defmacro defcomponent (class-name direct-superclasses component-name)
  `(progn (defclass ,class-name ,direct-superclasses
	    ((name :initform ,component-name)))
	  (setf (gethash ,component-name *component-classes*) ',class-name)))



(defcomponent gallery (double-bracket) "gallery")


(defcomponent size (double-bracket classified) "name")
(defcomponent code (double-bracket classified) "code")
(defcomponent collapsible (double-bracket classified) "collapsible")
(defcomponent note (double-bracket classified) "note")
(defcomponent html (double-bracket classified) "html")
(defcomponent span (double-bracket classified) "span")
(defcomponent div (double-bracket classified) "div")
(defcomponent div_ (double-bracket classified) "div_")
(defcomponent math (double-bracket classified) "math")
(defcomponent footnote (double-bracket classified) "footnote")
(defcomponent module (double-bracket classified) "module")
(defcomponent iftags (double-bracket classified) "iftags")
(defcomponent tabview (double-bracket classified) "tabview")
(defcomponent tab (double-bracket classified) "tab")
(defcomponent bibliography (double-bracket classified) "bibliography")



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
