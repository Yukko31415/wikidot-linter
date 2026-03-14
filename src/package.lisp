;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name


(defpackage #:wikidot-linter
  (:use #:cl)
  (:documentation "The wikidot-linter package.")
  (:export #:main))


(defpackage #:wdlinter-fifo-queue
  (:use #:cl)
  (:export #:make-queue
	   #:push-queue
	   #:pop-queue
	   #:print-queue-list))


(defpackage #:wdlinter-components
  (:use #:cl)
  (:shadow #:if
	   #:=
	   #:>
	   #:<)

  (:export

   #:*component-classes*

   #:toplevel

   #:component
   
   #:component-name
   #:component-bracketcount
   #:component-classp
   #:component-params
   #:component-content-queue
   #:component-end-name
   #:component-location

   #:single-bracket
   #:double-bracket
   #:triple-bracket

   #:classified
   #:unclassified

   #:unknown-component
   #:tag->component
   #:end-name=
   #:push-content
   #:end-tag-p

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



(defpackage #:wdlinter-parser
  (:use #:cl)
  (:import-from #:bind #:bind)
  (:local-nicknames (#:components #:wdlinter-components))
  (:export #:destruct-ftml-block))
