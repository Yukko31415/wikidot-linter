
;;;; --------
;;;; internal
;;;; --------


(in-package #:wdlinter-component.internal)


(defparameter *component-classes* (make-hash-table :test #'equal)
  "コンポーネント名をキーにクラスを保存するハッシュテーブル")


;; ----------------------------
;; defcomponent, component-list
;; ----------------------------

(defmacro defcomponent (class-name direct-superclasses component-name
		 &optional (end-name nil end-name-p))
  "defcomponent class-name ({superclass-name}*) component-name [end-name] => class-name"
  (let ((slots `((name :initform ,component-name))))
    ;; スーパークラスのリストに 'classified が含まれている場合のみ、
    ;; end-name のスロット定義をリストに追加する
    ;; end-nameが特殊に指定されている場合はそれを用いる
    (when (or (member 'classified direct-superclasses)
	     (member 'flexible direct-superclasses))
      (push `(end-name :initform ,(if end-name-p end-name
				      (format nil "/~A" component-name)))
	    slots))
    `(progn
       (defclass ,class-name ,direct-superclasses ,slots
	 (:documentation ,(format nil "The ~A component class" class-name)))
       (setf (gethash ,component-name *component-classes*) ',class-name))))


(defmacro component-list (direct-superclasses list)
  "component-list ({superclass-name}*) ({component}*) => <no values>
   component ::= (component-name tagname [end-name])
   superclass-name = a non-nil symbol."
  (let ((defcomponents
	  (loop :for (name component-name end-name) :in list
		:collect (if end-name
			     `(defcomponent ,name ,direct-superclasses
				  ,component-name ,end-name)
			     `(defcomponent ,name ,direct-superclasses
				  ,component-name)))))
    `(progn ,@defcomponents
	    (values))))






;;;; ------------------
;;;; wdlinter-component
;;;; ------------------


(in-package #:wdlinter-component)


(defclass toplevel ()
  ((content-queue
    :initform (fifo-queue:make-queue)
    :reader component-content-queue))
  (:documentation "トップレベル層を定義するクラス"))

(defclass component ()
  ((classp :reader component-classp)
   (name :reader component-name)
   (bracketcount :reader component-bracketcount)
   (params :initarg :params :reader component-params))
  (:documentation "各コンポネーントを定義する親クラス"))

(setf (documentation 'component-name 'function)
   "component-name component => string")

;;
;; bracket


(defclass single-bracket (component)
  ((bracketcount :initform 1))
  (:documentation "一重角括弧によって構成されるコンポーネント"))


(defclass double-bracket (component)
  ((bracketcount :initform 2))
  (:documentation "二重角括弧によって構成されるコンポーネント"))

(defclass triple-bracket (component)
  ((bracketcount :initform 3))
  (:documentation "三重角括弧によって構成されるコンポーネント"))

;;
;; classfied, unclassfied


(defclass classified (component)
  ((classp :initform t)
   (end-name :reader component-end-name)
   (content-queue :initform (fifo-queue:make-queue)
		  :reader component-content-queue))
  (:documentation "階層構造を構成するコンポネーント"))

(defclass unclassified (component)
  ((classp :initform nil))
  (:documentation "階層構造を構成しないコンポネーント"))

(defclass flexible (classified unclassified) ()
  (:documentation "階層構造を構成する場合があるコンポネーント"))



;;
;; util


(define-condition unknown-component (error)
  ((tagname :initarg :tagname :reader tagname))
  (:report (lambda (c s) (format s "\"~A\"は存在しないコンポーネントです"
			    (tagname c))))
  (:documentation "コンポーネントが発見できない場合に発生するエラー"))

(defun tag->component (tagname)
  "tag->component tagname => component
   tagname = a string designator."
  (when tagname
    (multiple-value-bind (component find)
	(gethash (string-downcase tagname) *component-classes*)
      (common-lisp:if find component
		      (error 'unknown-component :tagname tagname)))))

(defun end-name= (component end-tagname &key (start 0) end)
  "end-name= component end-tagname &key start end => {t | nil}
   component = a component class.
   end-tagname = a string designator.
   start, end = bounding index designators of sequence. The default for end is nil."
  (string= end-tagname (component-end-name component)
	   :start1 start :end1 end))

(defun end-tag-p (end-tagname &key (start 0))
  "end-tag-p end-tagname &key start => {t | nil}
   end-tagname = a string designator.
   start = a bounding index designator."
  (string= end-tagname "/" :start1 start :end1 (1+ start)))

(defun push-content (obj component)
  "push-content obj component => component"
  (fifo-queue:push-queue obj (component-content-queue component)))

(defun merge-content-queue (component content-queue)
  "merge-content-queue component content-queue => component"
  (fifo-queue:merge-queue (component-content-queue component)
			  content-queue))




;;;; --------------
;;;; component-list
;;;; --------------


;;
;; double-bracket, classified

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



;;
;; double-bracket, unclassified

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


;;
;; modules


(defcomponent module (double-bracket) "module")

(component-list
 (double-bracket classified)
 ((module/countpages "module")
  (module/listusers "module")
  (module/pagecalendar "module")
  (module/css "module")
  (module/css "module")
  (module/newpage "module")
  (module/redirect "module")
  (module/themepreviewer "module")
  (module/mailform "module")
  (module/featuredsite "module")
  (module/feed "module")
  (module/frontforum "module")
  (module/nextpage "module")
  (module/previospage "module")))


(component-list
 (double-bracket unclassified)
 ((module/listdrafts "module")
  (module/tagcloud "module")
  (module/pagetree "module")
  (module/backlinks "module")
  (module/wantedpages "module")
  (module/orphanedpages "module")
  (module/categories "module")
  (module/watchers "module")
  (module/members "module")
  (module/join "module")
  (module/sendinvitations "module")
  (module/whoinvited "module")
  (module/petitionadmin "module")
  (module/sitegrid "module")
  (module/comments "module")
  (module/recentposts "module")
  (module/minirecentthreads "module")
  (module/minirecentthreads "module")
  (module/miniactivethreads "module")
  (module/minirecentposts "module")
  (module/ratedpages "module")
  (flickrgallery "flickrgallery")
  (module/files "module")
  (module/search "module")
  (module/searchall "module")
  (module/searchusers "module")
  (module/sitechanges "module")
  (module/managesite "module")
  (module/clone "module")
  (module/adsenseunit "module")
  (module/membershipbypassword "module")
  (module/pages "module")
  (module/pages "module")
  (module/childpages "module")
  (module/pagesbytag "module")
  (module/simpletodo "module")))


(component-list
 (double-bracket flexible)
 ((module/listpages "module")
  (module/rate "module")
  (gallery "gallery")))






