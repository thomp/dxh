;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DXH; Base: 10 -*-
(in-package :dxh)
;;;
;;; vars.lisp: variable definitions for DXH
;;;
(defparameter *styles*
  ;; HTML
  '((:link ("http://127.0.0.1:3000/xfsql/css/default-style.css" "text/css")))
  ;; FIREFOX
  ;;nil
  ;; UBIQUITY
  ;;(link-xhc :href "http://127.0.0.1/backplanejs-0-6-5-yui-2.8.0/assets/backplane.css" :rel "stylesheet")
  ;;'((:link ("http://127.0.0.1:3000/backplanejs/assets/backplane.css" "text/css")))
  ;; XSLTFORMS
  ;;'((:xml ("http://127.0.0.1:3000/xslt/xsltforms/xsltforms.xsl" "application/xml")) (:link ("http://127.0.0.1:3000/xfsql/css/default-style.css" "text/css")) )
 "Default styles which should be applied to a web page. A list with members each with the format (style-type style-details). If style-type is :LINK style-details should be a list specifying the href and MIME type attributes of <link>...</link>. If style-type is :STYLE, style-details should be a string corresponding to <style>...</style>. If style-type is :XML, style-details should be a list in the format described for a member of DAT-XML::*DEFAULT-STYLESHEET-URLS* "
  )

;; deprecated (see above)
(defvar *default-stylesheet-url* nil
  "NIL or a string representing the default URL for a CSS stylesheet.")

(defvar *embed-css?* t
  "DEPRECATED. If true, instead of using <link ...> or <?xml-stylesheet ... > to point at a CSS file, embed the style information at *DEFAULT-STYLESHEET-URL* in the document itself. note(s): only URLs of the form file:///path/to/foo.css are currently supported"
  )

;; T: use <link ...>; NIL: use <?xml-stylesheet ...>"
(defvar *link-css-style?* nil
  "If true, include a <link ...> phrase in <head>...</head>.")

;; FIXME: ** what is this doing here? **
(defparameter *xfnamespace* "xf"
  "String representing the xforms namespace. Default value is 'xf'.")

(defvar *default-head-content* 'default-head-content-xhc)

(defvar *default-xh-top*
  'default-xh-top-content
  "Symbol representing function which should be called to generate default content at the top of any xhtml page.")

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



