(in-package :dxh)
;;;
;;; vars.lisp: variable definitions for DXH
;;;

;; FIXME: ** what is this doing here? **
(defparameter *xfnamespace* "xf"
  "String representing the xforms namespace. Default value is 'xf'.")

(defvar *default-head-content* 
  nil ; 'default-head-content-xhc
  "Symbol representing function which should be called to generate default content in HEAD component."
  )

;; test:
;; (let ((dxh:*xhtml-namespace* nil)) (dxh:html-xhc "joejoe"))
(defparameter *xhtml-namespace*
  "" ; "h"
  "Either NIL or a string representing the namespace for xhtml content. For example, 'h' would indicate that <p> should be rendered in an xhtml document as <h:p>.")

;; for xforms-tables
;; this should be redefined for application of interest
(defparameter model-item-properties
  (list
   ;; table name and alist of property/value pairs
   (cons "tests"
	 (list
	  (cons "curve" 
		(list
		 "readonly=\"boolean-from-string(if( ../curvelocked = 'T', 'true', 'false'))\"")
		)
	  (cons "curvetype" 
		(list
		 "readonly=\"boolean-from-string(if( ../curvelocked = 'T', 'true', 'false'))\"")
		)
	  )
	 ))
  "Model item properties (see xforms 1.1 section 6) for an item in a xforms model. Organized in a restrictive manner (for use with ssqlfs) as an alist with keys being 'table names' and values being alists with keys of 'column names'. Associated with a given table/columnlabel is a list of strings, 'propert=value' statement(s).")



