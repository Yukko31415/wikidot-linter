;;; wikidot-linter.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name

(asdf:defsystem #:wikidot-linter
  :description "A basic application."
  :author      "Your Name"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  ("cl-ppcre" "serapeum" "alexandria" "metabang-bind")
  :serial t
  :components ((:file "src/package")
	       (:file "src/fifo-queue")
	       (:file "src/component")
	       (:file "src/parser")
               (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "wdlinter"
  :entry-point "wikidot-linter:main")
