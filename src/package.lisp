;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name


(uiop:define-package #:wikidot-linter
  (:use #:cl)
  (:documentation "The wikidot-linter package.")
  (:export #:main))


(uiop:define-package #:wdlinter-fifo-queue
  (:use #:cl)
  (:import-from #:bind #:bind)
  (:export #:make-queue
	   #:merge-queue
	   #:pop-queue
	   #:print-queue-list
	   #:push-queue)
  (:export #:queue))


(uiop:define-package #:wdlinter-component.internal
  (:use #:cl)
  (:export #:*component-classes*)
  (:export #:find-component-name
	   #:find-trie-tree
	   #:make-trie-tree))


(uiop:define-package #:wdlinter-component
  (:use #:cl #:wdlinter-component.internal)
  (:local-nicknames (#:fifo-queue #:wdlinter-fifo-queue))
  (:shadow #:if #:= #:> #:<)
  (:export #:unknown-component)
  (:export #:component-name
	   #:end-name=
	   #:end-tag-p
	   #:merge-content-queue
	   #:push-content
	   #:tag->component)
  (:export #:*user
	   #:<
	   #:<image
	   #:=
	   #:=image
	   #:>
	   #:>image
	   #:a
	   #:a_
	   #:bibliography
	   #:button
	   #:cell
	   #:classified
	   #:code
	   #:collapsible
	   #:component
	   #:date
	   #:div
	   #:div_
	   #:double-bracket
	   #:eref
	   #:expr
	   #:f<image
	   #:f<toc
	   #:f>image
	   #:f>toc
	   #:file
	   #:flexible
	   #:flickrgallery
	   #:footnote
	   #:footnoteblock
	   #:gallery
	   #:hashtag
	   #:html
	   #:if
	   #:iftags
	   #:image
	   #:include
	   #:li
	   #:math
	   #:module
	   #:module/adsenseunit
	   #:module/backlinks
	   #:module/categories
	   #:module/childpages
	   #:module/clone
	   #:module/comments
	   #:module/countpages
	   #:module/css
	   #:module/featuredsite
	   #:module/feed
	   #:module/files
	   #:module/frontforum
	   #:module/join
	   #:module/listdrafts
	   #:module/listpages
	   #:module/listusers
	   #:module/mailform
	   #:module/managesite
	   #:module/members
	   #:module/membershipbypassword
	   #:module/miniactivethreads
	   #:module/minirecentposts
	   #:module/minirecentthreads
	   #:module/newpage
	   #:module/nextpage
	   #:module/orphanedpages
	   #:module/pagecalendar
	   #:module/pages
	   #:module/pagesbytag
	   #:module/pagetree
	   #:module/petitionadmin
	   #:module/previospage
	   #:module/rate
	   #:module/ratedpages
	   #:module/recentposts
	   #:module/redirect
	   #:module/search
	   #:module/searchall
	   #:module/searchusers
	   #:module/sendinvitations
	   #:module/simpletodo
	   #:module/sitechanges
	   #:module/sitegrid
	   #:module/tagcloud
	   #:module/themepreviewer
	   #:module/wantedpages
	   #:module/watchers
	   #:module/whoinvited
	   #:note
	   #:row
	   #:single-bracket
	   #:size
	   #:social
	   #:span
	   #:tab
	   #:table
	   #:tabview
	   #:toc
	   #:toplevel
	   #:triple-bracket
	   #:ul
	   #:unclassified
	   #:user))


(uiop:define-package #:wdlinter-parser
  (:use #:cl)
  (:import-from #:bind #:bind)
  (:local-nicknames (#:component #:wdlinter-component))
  (:local-nicknames (#:fifo-queue #:wdlinter-fifo-queue))
  (:export #:destruct-ftml-block))
