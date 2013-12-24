;;;
;;; dxh.asd
;;;
(defsystem dxh
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "vars")
	       (:file "xhtml")
	       (:file "auto")
	       (:file "xhtml-stream")
	       (:file "xhtml-string"))
  :depends-on (:url-rewrite
	       :dxg))
