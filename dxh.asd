;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; dxh.asd --- dxh system definition
;;;
(defsystem dxh
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "vars")
	       (:file "xhtml"))
  :depends-on (:url-rewrite	     ; which depends on...nothing!
	       ;; an XHTML document is a XML document
	       ;; - XML declaration, XML stylesheet specification, DOCTYPE declaration...
	       :dxg
	       ))
