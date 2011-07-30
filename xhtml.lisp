(in-package :dxh)
;;;
;;; xhtml.lisp: code for generating generic xhtml
;;;

;;;
;;; prefixes/suffix conventions:
;;;
;;;   -xhc
;;;     - an xhtml document component
;;;   -xh
;;;     - an xhtml document
;;;
;;;   -<xhc
;;;     - start tag only
;;;
;;;   html-
;;;     - generate larger structural block (e.g., HTML-TABLE, HTML-LIST
;;;

(defun a-xhc (string &key id href rwname rwval style stream target)
  "Return <a> component of html document to stream STREAM. Rewrite URL with RWNAME (name) and RWVAL (value), if both are non-nil."
  (declare (string string))
  (let ((newhref
	 (if (and rwname rwval)
	     (url-rewrite:add-get-param-to-url href rwname rwval)
	     href)))
    (xhc "a" string
	 :id id
	 :style style
	 :stream stream
	 :attributes
	 (list (list "href" newhref)
	       (list "target" target)))))

(defun b-xhc (some-string &key stream)
  (xhc "b" some-string :stream stream))

(defun body-xhc (some-string &key stream)
  "Return as a string the body of a xhtml document where SOME-STRING is included verbatim in the <body> component of the xhtml document. Output is sent to stream STREAM."
  (assert (stringp some-string) nil "SOME-STRING must be a string")
  (xhc "body" some-string :stream stream))

(defun br-xhc (&key id stream)
  (xhc "br" nil :id id :stream stream))

(defun button-xhc (some-string &key class disabled id onblur onclick onfocus style stream type name value)
  "Return a string."
  (xhc "button" some-string
       :attributes (list 
		    (list "disabled" disabled)
		    (list "name" name)
		    (list "type" type)
		    (list "value" value) 
		    (list "onclick" onclick)
		    (list "onfocus" onfocus)
		    (list "onblur" onblur))
       :class class
       :id id
       :style style
       :stream stream))

(defun caption-xhc (some-string &key stream)
  "Return as a string <title>...</title>."
  (xhc "caption" some-string :stream stream))

(defun col-xhc (some-string &key width)
  (xhc-protected "col" 
		 :attributes (list (list "width" width))
		 :protected-string (if some-string some-string "")))

(defun colgroup-xhc (some-string)
  (xhc-protected "colgroup" :protected-string some-string))

(defun default-head-content (&optional stream)
  "Return a string by calling the function pointed to by *default-head-content*. Send string to STREAM."
  (write-string 
   (if *default-head-content*
       (funcall *default-head-content*)
       "") 
   stream))

(defun div-xhc (some-string &key class id style stream)
  "Return as a string a <div>...</div> component of a xhtml document where SOME-STRING is included verbatim."
  (xhc "div" some-string :class class :id id :style style :stream stream))

(defun form-xhc (some-string &key class enctype style stream action name method)
  "METHOD should be a keyword (e.g., :POST) corresponding to the form method attribute."
  (xhc "form"
       some-string
       :attributes (list 
		    (list "enctype" enctype)
		    (list "method" (if method (string method)))
		    (list "action" action)
		    (list "name" name))
       :class class
       :style style
       :stream stream))

(defun h1-xhc (some-string &key class style stream)
  "Return as a string a <h1>...</h1> component of a xhtml document where SOME-STRING is included verbatim."
  (xhc "h1" some-string :class class :style style :stream stream))

(defun h2-xhc (some-string &key class stream)
  (xhc "h2" some-string :class class :stream stream))

(defun h3-xhc (some-string &key class style stream)
  "Return as a string a <h1>...</h1> component of a xhtml document where SOME-STRING is included verbatim."
  (xhc "h3" some-string :class class :style style :stream stream))

(defun head-xhc (some-string &key (defaults-p t) title stream)
  "Return the string corresponding to the <head>...</head> component of a xhtml document. If STREAM isn't NIL, send the string returned to stream STREAM. The string TITLE, if non-nil, is a string which specifies the <title> child of <head>. SOME-STRING is included verbatim as a child of the <head> node.

If DEFAULTS-P is nil, don't include default xhtml content (see DEFAULT-HEAD-CONTENT) in the component. See *STYLES* for defining default stylesheets."
  (write-string
   (xhc-protected 
    "head"
    :protected-string
    (with-output-to-string (s) 
      (if defaults-p (default-head-content s))
      (if title (title-xhc title :stream s))
      (if (stringp some-string)
	  (write-string some-string s))))
   stream))

(defun hr-xhc (&key stream style)
  (xhc "hr" nil :stream stream :style style))

(defun html-list (items &key (type :ul) stream)
  "Return a string, representing a list. Send string to STREAM."
  (apply
   (cond ((eq type :ul) 'ul-xhc)
	 ((eq type :ol) 'ol-xhc)
	 (t 'ul-xhc))
   (list (with-output-to-string (s)
	   (dolist (i items)
	     (li-xhc i :stream s)))
	 :stream stream)))

(defun html-<xhc (&key attributes stream)
  "Return start tag corresponding to <html ...>. ATTRIBUTES is a list of lists of the form (<attribute-name> <value>)"
  (dxg:start-tag "html" :attributes attributes :namespace *xhtml-namespace* :stream stream))

(defun html->xhc (&key stream)
  "Return start tag corresponding to <html ...>. ATTRIBUTES is a list of lists of the form (<attribute-name> <value>)"
  (dxg:end-tag "html" :namespace *xhtml-namespace* :stream stream))

;; FIXME: more sensical to have as args (head-content body-content ...) instead of (some-string ...)?
(defun html-xhc (some-string &key attributes stream)
  (write-string
   (concatenate 'string
    (html-<xhc
     :attributes
     (cond ((not attributes)
	    ;; default to xhtml
	    (if *xhtml-namespace*
		(list (list 
		       (if (string= "" *xhtml-namespace*)
			   "xmlns"
			   (format nil "xmlns:~A" *xhtml-namespace*))
		       "http://www.w3.org/1999/xhtml"))))
	   (t attributes)))
    some-string
    (html->xhc))
   stream))
  
(defun img-xhc (&key (alt "ALT not specified.") class src stream style)
  "Return a string."
  (xhc "img" ""
       :attributes (list (list "alt" alt)
			 (list "src" src))
       :class class
       :style style
       :stream stream))

(defun input-xhc (&key class id onblur onfocus readonly style stream type name value)
  "Return a string."
  (xhc "input" ""
       :attributes (list 
		    (list "readonly" readonly)
		    (list "type" type)
		    (list "name" name)
		    (list "value" value)
		    (list "onfocus" onfocus)
		    (list "onblur" onblur))
       :class class
       :id id
       :style style
       :stream stream))

(defun li-xhc  (some-string &key class style stream)
  "Return as a string a <li>...</li> component of a xhtml document where SOME-STRING is included verbatim."
  (xhc "li" some-string :class class :style style :stream stream))

(defun link-xhc (&key href type rel stream)
  (xhc "link" 
       nil
       :attributes (list (list "href" href)
			 (list "rel" rel)
			 (list "type" type))
       :stream stream))

(defun ol-xhc  (some-string &key class id style stream)
  (xhc "ol" some-string :class class :id id :style style :stream stream))

(defun option-xhc (some-string &key class label selected style stream value)
  (xhc "option" some-string 
        :attributes (list 
		     (list "label" label)
		     (list "value" value)
		     (list "selected" selected))
       :class class :style style :stream stream))

(defun p-xhc (some-string &key class style stream)
  (xhc "p" some-string :class class :style style :stream stream))

(defun pre-xhc (some-string &key class id style stream)
  (xhc "pre" some-string :class class :id id :style style :stream stream))

(defun script-xhc (some-string &key (protect-with-cdata-p t) src stream type)
  "TYPE is a string (e.g., 'text/javascript') representing the type attribute for the script tag."
  (xhc "script"
       (if protect-with-cdata-p
	   ;; CDATA needed with some browsers (e.g., firefox 3.5)
	   (concatenate 'string "/* <![CDATA[ */
"
		  some-string
		  "
/* ]]> */")
	   some-string)
       :stream stream :attributes (list 
				   (list "src" src)
				   (list "type" type))))

(defun select-xhc (some-string &key class id multiple name onchange style stream)
  "Return as a string a select component of a xhtml document where SOME-STRING is included verbatim."
  (xhc "select" some-string 
        :attributes (list 
		     (list "multiple" multiple)
		     (list "name" name)
		     (list "onchange" onchange))
       :class class :id id :style style :stream stream))

(defun style-xhc (some-string &key protect-with-cdata-p stream (type "text/css"))
  "TYPE is a string representing the type attribute."
  (xhc "style"
       (if protect-with-cdata-p
	   ;; CDATA needed with some browsers (e.g., firefox 3.5)
	   (concatenate 'string "/* <![CDATA[ */
"
		  some-string
		  "
/* ]]> */")
	   some-string) 
       :attributes (list (list "type" type))
       :stream stream))

(defun span-xhc (some-string &key style class stream)
  (xhc "span" some-string :style style :class class :stream stream))

(defun sub-xhc (some-string &key style class stream)
  (xhc "sub" some-string :style style :class class :stream stream))

(defun sup-xhc (some-string &key id style class stream)
  (xhc "sup" some-string :id id :style style :class class :stream stream))

(defun table-xhc (some-string &key stream attributes style class)
  "Return a string."
  (xhc "table" some-string
       :attributes attributes
       :class class
       :style style
       :stream stream))

(defun tbody-xhc (some-string &key stream attributes style class)
  "Return a string."
  (xhc "tbody" some-string
       :attributes attributes
       :class class
       :style style
       :stream stream))

(defun td-xhc (some-string &key stream)
  "Return a string."
  (xhc "td" some-string :stream stream))

(defun textarea-xhc (some-string &key class id onblur onfocus readonly style stream type name value)
  "Return a string."
  (xhc "textarea" some-string
       :attributes (list 
		    (list "readonly" readonly)
		    (list "type" type)
		    (list "name" name)
		    (list "value" value)
		    (list "onfocus" onfocus)
		    (list "onblur" onblur))
       :class class
       :id id
       :style style
       :stream stream))

(defun th-xhc (some-string &key stream)
  "Return a string."
  (xhc "th" some-string :stream stream))

(defun thead-xhc (some-string &key stream attributes style class)
  "Return a string."
  (xhc "thead" some-string
       :attributes attributes
       :class class
       :style style
       :stream stream))

(defun title-xhc (some-string &key stream)
  "Return as a string <title>...</title>."
  (xhc "title" some-string :stream stream))

(defun tr-xhc (some-string &key stream)
  "Return a string."
  (xhc "tr" some-string :stream stream))

(defun ul-xhc (some-string &key id stream)
  (xhc "ul" some-string :id id :stream stream))

;; FIXME: use DXG:XMLC
(defun xhc (tag some-string &key attributes class id style stream)
  "Return as a string a <tag>...</tag> component of a xhtml document where SOME-STRING is included verbatim as the value of the node. If SOME-STRING is NIL, return <tag ... />. ATTRIBUTES is an alist of strings where car is attribute and cadr is attribute value (see DXG:XMLC). If STREAM is non-NIL, write string to stream STREAM."
  (assert (stringp tag))
  (assert (listp attributes))
  ;;(assert (dat-cl-utils:noas some-string) nil (format nil "XHC: SOME-STRING should be a string or nil; instead it was ~A." some-string))
  (let ((attributes
	 (append (list (list "class" class)
		       (list "style" style)
		       (list "id" id))
		 attributes))
	;; (return-string
	;;  (if some-string
	;;      (concatenate 'string
	;;       (dxg:start-tag 
	;; 	tag
	;; 	:verbatim-attributes attributes-string 
	;; 	:namespace *xhtml-namespace*)
	;;       some-string 
	;;       (dxt:end-tag tag :namespace *xhtml-namespace*))
	;;      ;; FIXME: should be able to pass verbatim attributes here
	;;      (dxg:empty-tag tag 
	;; 			 :namespace *xhtml-namespace* 
	;; 			 :attributes-string attributes-string))) 
	)
    (dxg:xmlc tag some-string
	      :attr attributes
	      :namespace *xhtml-namespace*
	      :stream stream)))

(defun xhc-protected (label &key (protected-string "") attributes)
  "Return <label> XHTML component of an xforms/xhtml document. ATTRIBUTES is a list of lists. Each sublist has two members, the first is a string corresponding to the attribute and the second is a string corresponding to the attribute value. PROTECTED-STRING is a string which is included verbatim as a child of the <label>...</label> node."
  (assert (listp attributes))
  (assert (stringp label))
  (assert (stringp protected-string))
  (with-output-to-string (s)
    (dxg:start-tag label
			:attributes attributes
			:namespace *xhtml-namespace* :stream s)
    (write-string protected-string s)
    (dxg:end-tag label :namespace *xhtml-namespace* :stream s)))

;; FIXME: NIL stylesheets should correspond to document w/o stylesheets according to current docs... 
(defun xhdoc (body &key stylesheets title)
  "BODY is a string inserted verbatim inside <body>...</body>. STYLESHEETS is a list of lists. Each sublist contains, at the first position, a string representing a URL (corresponding to the 'href' attribute), and, at the second position, a string representing the stylesheet type (corresponding to the 'type' attribute. "
  (with-output-to-string (s)
    (dxg:xml-spec :stylesheets stylesheets :stream s)
    (html-xhc
     (with-output-to-string (s2)
       (head-xhc nil :title title :stream s2) 
       (body-xhc body :stream s2))
     :stream s)))