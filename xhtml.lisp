(in-package :dxh)
;;;
;;; xhtml.lisp: generate XHTML
;;;
(defun a (string &key attributes id href rwname rwval style stream target title)
  "Write <a>...</a> component to stream STREAM. Rewrite URL with RWNAME (name) and RWVAL (value), if both are non-nil."
  (declare (string string))
  (let ((newhref
	 (if (and rwname rwval)
	     (url-rewrite:add-get-param-to-url href rwname rwval)
	     href)))
    (xhc "a" string
	 :attributes
	 (append attributes
		 (list (list "href" newhref)
		       (list "target" target)
		       (list "title" title)))
	 :id id
	 :style style
	 :stream stream)))

(defun b (some-string &key attributes stream)
  (xhc "b" some-string :attributes attributes :stream stream))

(defun blockquote (some-string &key attributes class style stream)
  (xhc "blockquote" some-string :attributes attributes :class class :style style :stream stream))

(defun body (some-string &key attributes stream)
  "Return as a string the body \"<body>...</body>\" where SOME-STRING is included verbatim in the <body>...</body> component. Output is sent to stream STREAM."
  (assert (stringp some-string) nil "SOME-STRING must be a string")
  (xhc "body" some-string :attributes attributes :stream stream))

(defun br (&key attributes id stream)
  (xhc "br" nil :attributes attributes :id id :stream stream))

(defun button (some-string &key accesskey class disabled id onblur onclick onfocus style stream tabindex title type name value)
  "Return a string."
  (xhc "button" some-string
       :attributes (list 
		    (list "accesskey" accesskey)
		    (list "disabled" disabled)
		    (list "name" name) 
		    (list "tabindex" tabindex)
		    (list "title" title)
		    (list "type" type)
		    (list "value" value) 
		    (list "onclick" onclick)
		    (list "onfocus" onfocus)
		    (list "onblur" onblur))
       :class class
       :id id
       :style style
       :stream stream))

(defun caption (some-string &key attributes stream)
  (xhc "caption" some-string :attributes attributes :stream stream))

(defun code (some-string &key attributes style class stream)
  (xhc "code" some-string :attributes attributes :style style :class class :stream stream))

(defun col (some-string &key id stream width)
  (xhc "col" some-string :attributes (list (list "width" width)) :id id :stream stream))

(defun colgroup (some-string &key attributes)
  (xhc-protected "colgroup" :attributes attributes :protected-string some-string))

(defun default-head-content (&optional stream)
  "Return a string by calling the function pointed to by *default-head-content*. Send string to STREAM."
  (write-string 
   (if *default-head-content*
       (funcall *default-head-content*)
       "") 
   stream))

(defun div (some-string &key class id style stream)
  "Return as a string a <div>...</div> component of a xhtml document where SOME-STRING is included verbatim."
  (xhc "div" some-string :class class :id id :style style :stream stream))

(defun dd  (some-string &key attributes class style stream)
  (xhc "dd" some-string :attributes attributes :class class :style style :stream stream))

(defun dl  (some-string &key attributes class style stream)
  (xhc "dl" some-string :attributes attributes :class class :style style :stream stream))

(defun dt  (some-string &key attributes class style stream)
  (xhc "dt" some-string :attributes attributes :class class :style style :stream stream))

(defun em (some-string &key attributes stream)
  (xhc "em" some-string :attributes attributes :stream stream))

(defun form (some-string &key class enctype style stream action name method title)
  "METHOD should be a keyword (e.g., :POST) corresponding to the form method attribute."
  (xhc "form"
       some-string
       :attributes (list 
		    (list "enctype" enctype)
		    (list "method" (if method (string method)))
		    (list "action" action)
		    (list "name" name)
		    (list "title" title))
       :class class
       :style style
       :stream stream))

(defun h1 (some-string &key attributes class style stream)
  (xhc "h1" some-string :attributes attributes :class class :style style :stream stream))

(defun h2 (some-string &key attributes class stream)
  (xhc "h2" some-string :attributes attributes :class class :stream stream))

(defun h3 (some-string &key attributes class style stream)
  (xhc "h3" some-string :attributes attributes :class class :style style :stream stream))

(defun head (some-string &key (defaults-p t) title stream)
  "Return the string corresponding to the <head>...</head> component of a xhtml document. If STREAM isn't NIL, send the string returned to stream STREAM. The string TITLE, if non-nil, is a string which specifies the <title>...</title> child of <head>...</head>. SOME-STRING is included verbatim as a child of the <head>...</head> node.

If DEFAULTS-P is nil, don't include default XHTML content (see DEFAULT-HEAD-CONTENT) in the component."
  (write-string
   (xhc-protected 
    "head"
    :protected-string
    (with-output-to-string (s) 
      (if defaults-p (default-head-content s))
      (if title (title title :stream s))
      (if (stringp some-string)
	  (write-string some-string s))))
   stream))

(defun hr (&key attributes stream style)
  (xhc "hr" nil :attributes attributes :stream stream :style style))

(defun html-list (items &key (type :ul) stream)
  "Return a string, representing a list. Send string to STREAM."
  (apply
   (cond ((eq type :ul) 'ul)
	 ((eq type :ol) 'ol)
	 (t 'ul))
   (list (with-output-to-string (s)
	   (dolist (i items)
	     (li i :stream s)))
	 :stream stream)))

(defun html-<xhc (&key attributes stream)
  "Return start tag corresponding to <html ...>. ATTRIBUTES is a list of lists of the form (<attribute-name> <value>)"
  (dxg:start-tag "html" :attributes attributes :namespace *xhtml-namespace* :stream stream))

(defun html->xhc (&key stream)
  (dxg:end-tag "html" :namespace *xhtml-namespace* :stream stream))

(defun html (some-string &key attributes stream)
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

(defun html* (some-string &key attributes stream)
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
	 (t attributes))
   :stream stream)
  (write-string some-string stream)
  (html->xhc :stream stream))

(defun i  (some-string &key attributes class style stream)
  (xhc "i" some-string :attributes attributes :class class :style style :stream stream))

(defun img (&key (alt "ALT not specified.") class src stream style title)
  "Return a string."
  (xhc "img" ""
       :attributes (list (list "alt" alt)
			 (list "src" src)
			 (list "title" title))
       :class class
       :style style
       :stream stream))

(defun input (&key accesskey checked class id onblur onfocus readonly style stream type name tabindex title value)
  (xhc "input" ""
       :attributes (list 
		    (list "accesskey" accesskey)
		    (list "checked" checked)
		    (list "readonly" readonly)
		    (list "type" type)
		    (list "name" name)
		    (list "tabindex" tabindex)
		    (list "title" title)
		    (list "value" value)
		    (list "onfocus" onfocus)
		    (list "onblur" onblur))
       :class class
       :id id
       :style style
       :stream stream))

(defun li  (some-string &key attributes class style stream)
  (xhc "li" some-string :attributes attributes :class class :style style :stream stream))

(defun link (&key href type rel stream title)
  (xhc "link"
       nil
       :attributes (list (list "href" href)
			 (list "rel" rel)
			 (list "title" title)
			 (list "type" type))
       :stream stream))

(defun meta (some-string &key (content "text/html;charset=utf-8") (http-equiv "Content-Type") name scheme stream)
  (xhc "meta"
       some-string
       :attributes (list
		    (list "content" content)
		    (list "http-equiv" http-equiv)
		    (list "name" name)
		    (list "scheme" scheme))
       :stream stream))

(defun object (some-string &key data type stream) 
  (xhc "object"
       some-string
       ;; HTML 4.01 attributes: declare classid codebase data type codetype archive standby height width usemap name tabindex  
       :attributes (list
		    (list "data" data)
		    (list "type" type))
       :stream stream))

(defun ol (some-string &key attributes class id style stream)
  (xhc "ol" some-string :attributes attributes :class class :id id :style style :stream stream))

(defun option (some-string &key class label selected style stream title value)
  (xhc "option" some-string 
        :attributes (list 
		     (list "label" label)
		     (list "value" value)
		     (list "selected" selected)
		     (list "title" title))
       :class class :style style :stream stream))

(defun p (some-string &key attributes class style stream)
  (xhc "p" some-string :attributes attributes :class class :style style :stream stream))

(defun pre (some-string &key attributes class id style stream)
  (xhc "pre" some-string :attributes attributes :class class :id id :style style :stream stream))

(defun script (some-string &key (protect-with-cdata-p t) src stream type)
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
       :stream stream 
       :attributes (list 
		    (list "src" src)
		    (list "type" type))))

(defun select (some-string &key accesskey class id multiple name onchange style stream tabindex title)
  (xhc "select" some-string 
        :attributes (list 
		     (list "accesskey" accesskey)
		     (list "multiple" multiple)
		     (list "name" name)
		     (list "onchange" onchange)
		     (list "tabindex" tabindex)
		     (list "title" title))
       :class class :id id :style style :stream stream))

(defun span (some-string &key attributes style class stream)
  (xhc "span" some-string :attributes attributes :style style :class class :stream stream))

(defun strong (some-string &key attributes stream)
  (xhc "strong" some-string :attributes attributes :stream stream))

(defun style (some-string &key protect-with-cdata-p stream (type "text/css"))
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

(defun sub (some-string &key attributes style class stream)
  (xhc "sub" some-string :attributes attributes :style style :class class :stream stream))

(defun sup (some-string &key attributes id style class stream)
  (xhc "sup" some-string :attributes attributes :id id :style style :class class :stream stream))

(defun table (some-string &key stream attributes style class)
  (xhc "table" some-string
       :attributes attributes
       :class class
       :style style
       :stream stream))

(defun tbody (some-string &key stream attributes style class)
  (xhc "tbody" some-string
       :attributes attributes
       :class class
       :style style
       :stream stream))

(defun td (some-string &key attributes stream)
  (xhc "td" some-string :attributes attributes :stream stream))

(defun textarea (some-string &key accesskey class id onblur onfocus readonly style stream tabindex title type name value)
  (xhc "textarea" some-string
       :attributes (list
		    (list "accesskey" accesskey)
		    (list "readonly" readonly)
		    (list "type" type)
		    (list "name" name)
		    (list "tabindex" tabindex)
		    (list "value" value)
		    (list "onfocus" onfocus)
		    (list "onblur" onblur)
		    (list "title" title))
       :class class
       :id id
       :style style
       :stream stream))

(defun th (some-string &key attributes stream)
  "Return a string."
  (xhc "th" some-string :attributes attributes :stream stream))

(defun thead (some-string &key stream attributes style class)
  (xhc "thead" some-string
       :attributes attributes
       :class class
       :style style
       :stream stream))

(defun title (some-string &key attributes stream)
  (xhc "title" some-string :attributes attributes :stream stream))

(defun tr (some-string &key attributes stream)
  (xhc "tr" some-string :attributes attributes :stream stream))

(defun u (some-string &key attributes id stream)
  (xhc "u" some-string :attributes attributes :id id :stream stream))

(defun ul (some-string &key attributes id stream)
  (xhc "ul" some-string :attributes attributes :id id :stream stream))

(defun xhc (tag some-string &key attributes class id style stream)
  "Return as a string a <tag>...</tag> component of a xhtml document where SOME-STRING is included verbatim as the value of the node. If SOME-STRING is NIL, return <tag ... />. ATTRIBUTES is an alist of strings where car is attribute and cadr is attribute value (see DXG:XMLC). If STREAM is non-NIL, write string to stream STREAM."
  (declare (list attributes) 
	   (string tag))
  ;;(assert (stringp tag))
  ;;(assert (listp attributes))
  (dxg:xmlc tag some-string
	    :attr (append (list (list "class" class)
				(list "style" style)
				(list "id" id))
			  attributes)
	    :namespace *xhtml-namespace*
	    :stream stream))

(defun xhc* (tag some-string &key attributes class id style stream)
  "Return as a string a <tag>...</tag> component of a xhtml document where SOME-STRING is included verbatim as the value of the node. If SOME-STRING is NIL, return <tag ... />. ATTRIBUTES is an alist of strings where car is attribute and cadr is attribute value (see DXG:XMLC). If STREAM is non-NIL, write string to stream STREAM."
  (declare (list attributes) 
	   (string tag))
  (dxg:xmlc* tag some-string
	     :attr (append (list (list "class" class)
				 (list "style" style)
				 (list "id" id))
			   attributes)
	     :namespace *xhtml-namespace*
	     :stream stream))

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
  "BODY is a string inserted verbatim inside <body>...</body>. STYLESHEETS is a list of lists. Each sublist contains, at the first position, a string representing a URL (corresponding to the 'href' attribute), and, at the second position, a string representing the stylesheet type (corresponding to the 'type' attribute."
  (with-output-to-string (s)
    (dxg:xml-spec :stylesheets stylesheets :stream s)
    (html
     (with-output-to-string (s2)
       (head nil :title title :stream s2) 
       (body body :stream s2))
     :stream s)))