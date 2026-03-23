

#|---------- wdlinter-component.internal ----------|#



(in-package #:wdlinter-component.internal)

;; ------------------
;;;; make-trie-tree
;; ------------------

;;
;; trie-tree

(defstruct (trie-tree (:constructor %%make-trie-tree)
		      (:print-object print-trie-tree))
  key content)

(defun print-trie-tree (obj stream)
  (with-slots (key content) obj
    (format stream "(~A ~A)" key content)))

(defun %make-trie-tree (class-name component-name index &aux (length (length component-name)))
  (if (= index length)
      class-name
      (%%make-trie-tree
       :key (char component-name index)
       :content (list (%make-trie-tree class-name component-name (1+ index))))))

;;
;; make-trie-tree

(defun make-trie-tree (tree class-name component-name &aux (length (length component-name)))
  "make-trie-tree tree class-name component-name => tree"
  (labels ((gen-trie-tree (tree index)
	     (with-slots (key content) tree
	       (if (= length index) (pushnew class-name content :test #'eq)
		   (let ((new-tree (find-trie-tree (char component-name index) tree)))
		     (if new-tree (gen-trie-tree new-tree (1+ index))
			 (push (%make-trie-tree class-name component-name index) content)))))))
    (gen-trie-tree tree 0) tree))

;;
;; component-classes

(defparameter *component-classes* (%%make-trie-tree :key :component :content nil)
  "コンポーネント名をキーにクラスを保存するトライ木")

;;
;; find-trie-tree

(defun find-trie-tree (char tree)
  "find-trie-tree char tree => trie-tree
   trie-tree = a struct of trie-tree"
  (when tree (with-slots (key content) tree
	       (find char (remove-if #'symbolp content)
		     :key #'trie-tree-key :test #'char=))))

;; 
;; find-component-name

(defun find-component-name (tree)
  "find-component-name tree => symbol"
  (when tree (with-slots (key content) tree
	       (find-if #'symbolp content))))




#|---------- wdlinter-component ----------|#


(in-package #:wdlinter-component)


;; --------------------------------
;;;; defcomponent, component-list
;; --------------------------------


(defmacro defcomponent (class-name direct-superclasses component-name
			&optional (end-name nil end-name-p))
  ;; defcomponent class-name ({superclass-name}*) component-name [end-name] => class-name
  (let ((slots `((name :initform ,component-name))))
    ;; スーパークラスのリストに 'classified が含まれている場合のみ、
    ;; end-name のスロット定義をリストに追加する
    ;; end-nameが特殊に指定されている場合はそれを用いる
    (when (intersection direct-superclasses '(flexible classified))
      (push `(end-name :initform ,(cl:if end-name-p end-name
					 (format nil "/~A" component-name)))
	    slots))
    `(progn
       (defclass ,class-name ,direct-superclasses ,slots
	 (:documentation ,(format nil "The ~A component class" class-name)))
       (make-trie-tree *component-classes* ',class-name ,component-name))))


(defmacro component-list (direct-superclasses list)
  ;; component-list ({superclass-name}*) ({component}*) => <no values>
  ;; component ::= (component-name tagname [end-name])
  ;; superclass-name = a non-nil symbol.
  (let ((defcomponents
	  (loop :for (name component-name end-name) :in list
		:collect (cl:if end-name
				`(defcomponent ,name ,direct-superclasses
				     ,component-name ,end-name)
				`(defcomponent ,name ,direct-superclasses
				     ,component-name)))))
    `(progn ,@defcomponents
	    (values))))


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

(defclass flexible (component)
  ((classp :accessor component-classp)
   (end-name :reader component-end-name)
   (content-queue :initform (fifo-queue:make-queue)
		  :reader component-content-queue))
  (:documentation "階層構造を構成する場合があるコンポネーント"))



;;
;; util


(define-condition unknown-component (error)
  ((tagname :initarg :tagname :reader tagname))
  (:report (lambda (c s) (format s "\"~A\"は存在しないコンポーネントです"
			    (tagname c))))
  (:documentation "コンポーネントが発見できない場合に発生するエラー"))

(defun tag->component (string &key (start 0) end)
  "tag->component tagname &key start end => component
   tagname = a string designator.
   start, end = bounding index designators of sequence. The default for end is nil."
  (loop :with tree := *component-classes*
	:for index :from start :below (or end (length string))
	:for char := (aref string index)
	:collect char :into tagname
	:do (setf tree (find-trie-tree char tree))
	:finally (return (or (find-component-name tree)
			     (error 'unknown-component
				    :tagname (concatenate 'string tagname))))))



(defun end-name= (component end-tagname &key (start 0) end)
  "end-name= component end-tagname &key start end => {t | nil}
   component = a component class.
   end-tagname = a string designator."
  (string= end-tagname (component-end-name component)
	   :start1 start :end1 end))

(defun end-tag-p (end-tagname &optional (start 0) &aux (end (1+ start)))
  "end-tag-p end-tagname &optional start => {t | nil}
   end-tagname = a string designator."
  (string= "/" end-tagname :start2 start :end2 end))

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


(defcomponent module (double-bracket)  "module")

(component-list
 (classified module)
 ((module.countpages "countpages" "/module")
  (module.listusers "listusers" "/module")
  (module.pagecalendar "pagecalendar" "/module")
  (module.css "css" "/module")
  (module.newpage "newpage" "/module")
  (module.redirect "redirect" "/module")
  (module.themepreviewer "themepreviewer" "/module")
  (module.mailform "mailform" "/module")
  (module.featuredsite "featuredsite" "/module")
  (module.feed "feed" "/module")
  (module.frontforum "frontforum" "/module")
  (module.nextpage "nextpage" "/module")
  (module.previospage "previospage" "/module")))


(component-list
 (unclassified module)
 ((module.listdrafts "listdrafts" "/module")
  (module.tagcloud "tagcloud" "/module")
  (module.pagetree "pagetree" "/module")
  (module.backlinks "backlinks" "/module")
  (module.wantedpages "wantedpages" "/module")
  (module.orphanedpages "orphanedpages" "/module")
  (module.categories "categories" "/module")
  (module.watchers "watchers" "/module")
  (module.members "members" "/module")
  (module.join "join" "/module")
  (module.sendinvitations "sendinvitations" "/module")
  (module.whoinvited "whoinvited" "/module")
  (module.petitionadmin "petitionadmin" "/module")
  (module.sitegrid "sitegrid" "/module")
  (module.comments "comments" "/module")
  (module.recentposts "recentposts" "/module")
  (module.minirecentthreads "minirecentthreads" "/module")
  (module.minirecentposts "minirecentposts" "/module")
  (module.ratedpages "ratedpages" "/module")
  (module.flickrgallery "flickrgallery" "/module")
  (module.files "files" "/module")
  (module.search "search" "/module")
  (module.searchall "searchall" "/module")
  (module.searchusers "searchusers" "/module")
  (module.sitechanges "sitechanges" "/module")
  (module.managesite "managesite" "/module")
  (module.clone "clone" "/module")
  (module.adsenseunit "adsenseunit" "/module")
  (module.membershipbypassword "membershipbypassword" "/module")
  (module.pages "pages" "/module")
  (module.childpages "childpages" "/module")
  (module.pagesbytag "pagesbytag" "/module")
  (module.simpletodo "simpletodo" "/module")))


(component-list
 (flexible module)
 ((module.listpages "listpages" "/module")
  (module.rate "rate" "/module")
  (module.gallery "gallery")))








