(in-package :dxh)
;;;
;;; xhtml.lisp: generate XHTML
;;;
(defun colgroup (some-string &key attributes)
  (xhc-protected "colgroup" :attributes attributes :protected-string some-string))

(defun default-head-content (&optional stream)
  "Return a string by calling the function pointed to by *default-head-content*. Send string to STREAM."
  (write-string 
   (if *default-head-content*
       (funcall *default-head-content*)
       "") 
   stream))

(defun head (some-string &key (defaults-p t) title stream)
  "Return the string corresponding to the <head>...</head> component of a xhtml document. If STREAM isn't NIL, send the string returned to stream STREAM. The string TITLE, if non-nil, is a string which specifies the <title>...</title> child of <head>...</head>. SOME-STRING is included verbatim as a child of the <head>...</head> node.

If DEFAULTS-P is nil, don't include default XHTML content (see DEFAULT-HEAD-CONTENT) in the component."
  (write-string
   (xhc-protected 
    "head"
    :protected-string
    (with-output-to-string (s) 
      (if defaults-p (default-head-content s))
      (if title (title title s))
      (if (stringp some-string)
	  (write-string some-string s))))
   stream))

(defun html-list (items &key (type :ul) stream)
  "Return a string, representing a list. Send string to STREAM."
  (apply
   (cond ((eq type :ul) 'ul)
	 ((eq type :ol) 'ol)
	 (t 'ul))
   (list (with-output-to-string (s)
	   (dolist (i items)
	     (li i s)))
	 stream)))

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

(defun xhc+ (tag f stream &key attributes class id style)
  (declare (list attributes) 
	   (string tag))
  (dxg::elt+ tag f stream
	     :attributes (append (list (list "class" class)
				 (list "style" style)
				 (list "id" id))
			   attributes)
	     :namespace *xhtml-namespace*))

(defun xhc* (tag some-string &key attributes class id style stream)
  (declare (list attributes) 
	   (string tag))
  (dxg:xmlc* tag some-string stream
	     :attr (append (list (list "class" class)
				 (list "style" style)
				 (list "id" id))
			   attributes)
	     :namespace *xhtml-namespace*))

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
     #'(lambda (s2)
	 (head nil :title title :stream s2) 
	 (body body s2))
     :stream s)))