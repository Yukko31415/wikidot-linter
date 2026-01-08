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
  :depends-on  ("cl-ppcre" "serapeum" "log4cl" "alexandria")
  :serial t
  :components ((:file "src/package")
	       (:file "src/wikilinter-components")
	       (:file "src/wikilinter-fifo-queue")
	       (:file "src/wikilinter-parser")
               (:file "src/main")))





