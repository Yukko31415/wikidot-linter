;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name



(defpackage #:wikidot-linter
  (:use #:cl)
  (:documentation "The wikidot-linter package.")
  (:export #:main))


(defpackage #:wikilinter-parser
  (:use #:cl))


(defpackage #:wikilinter-fifo-queue
  (:use #:cl)
  (:export #:make-fifo-queue-handler))



(defpackage #:wikilinter-components
  (:use #:cl)
  (:shadow #:if
	   #:=
	   #:>
	   #:<)

  (:export
   
   #:*component-classes*

   #:component-name
   #:component-bracketcount
   #:component-classp
   #:component-params
   #:component-content-queue-handler
   #:component-end-name
   #:component-location

   #:single-bracket
   #:double-bracket
   #:triple-bracket
   #:classified
   #:unclassified
   
   #:toplevel

   #:tag->component
   
   #:gallery
   
   #:size
   #:code
   #:collapsible
   #:note
   #:html
   #:span
   #:div
   #:div_
   #:math
   #:footnote
   #:module
   #:iftags
   #:tabview
   #:tab
   #:bibliography
   #:a
   #:a_
   #:=
   #:>
   #:<
   #:ul
   #:li
   #:table
   #:row
   #:cell


   
   #:toc
   #:f>toc
   #:f<toc
   #:image
   #:=image
   #:<image
   #:>image
   #:f<image
   #:f>image
   #:eref
   #:footnoteblock
   #:include
   #:date
   #:file
   #:user
   #:*user
   #:social
   #:button
   #:expr
   #:if))
