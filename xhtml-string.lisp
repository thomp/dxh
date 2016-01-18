(in-package :dxh)
;;;
;;; xhtml-string.lisp: 
;;;     functions which return a string representing an HTML element
;;;
(defun a-s (string &key attributes class id href rwname rwval style target title)
  "Rewrite URL with RWNAME (name) and RWVAL (value), if both are non-nil."
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
	 :class class
	 :id id
	 :style style)))

(defun button-s (some-string &key accesskey class disabled id onblur onclick onfocus style tabindex title type name value)
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
       :style style))

(defun col-s (some-string &key id width)
  (xhc "col" some-string :attributes (list (list "width" width)) :id id))

(defun form-s (some-string &key class enctype style action name method title)
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
       :style style))

(defun img-s (&key (alt "ALT not specified.") class src style title)
  (xhc "img" ""
       :attributes (list (list "alt" alt)
			 (list "src" src)
			 (list "title" title))
       :class class
       :style style))

(defun input-s (&key accesskey checked class id onblur onfocus readonly style type name tabindex title value)
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
       :style style))

(defun label-s (some-string &key class for style)
  (xhc "label" some-string 
       :attributes (list (list "for" for)) 
       :class class :style style))

(defun link-s (&key href type rel title)
  (xhc "link"
       nil
       :attributes (list (list "href" href)
			 (list "rel" rel)
			 (list "title" title)
			 (list "type" type))))

(defun meta-s (some-string &key (charset "utf-8") content http-equiv name scheme)
  (xhc "meta"
       some-string
       :attributes (list
		    (list "charset" charset)
		    (list "content" content)
		    (list "http-equiv" http-equiv)
		    (list "name" name)
		    (list "scheme" scheme))))

(defun object-s (some-string &key data type) 
  (xhc "object"
       some-string
       ;; HTML 4.01 attributes: declare classid codebase data type codetype archive standby height width usemap name tabindex  
       :attributes (list
		    (list "data" data)
		    (list "type" type))))

(defun option-s (some-string &key class label selected style title value)
  (xhc "option" some-string 
        :attributes (list 
		     (list "label" label)
		     (list "value" value)
		     (list "selected" selected)
		     (list "title" title))
       :class class :style style))

(defun select-s (some-string &key accesskey class id multiple name onchange size style tabindex title)
  (xhc "select" some-string 
        :attributes (list 
		     (list "accesskey" accesskey)
		     (list "multiple" multiple)
		     (list "name" name)
		     (list "onchange" onchange)
		     (list "size" size)
		     (list "tabindex" tabindex)
		     (list "title" title))
       :class class :id id :style style))

(defun style-s (some-string &key protect-with-cdata-p (type "text/css"))
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
       :attributes (list (list "type" type))))

(defun textarea-s (some-string &key accesskey class id onblur onfocus readonly style tabindex title type name value)
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
       :style style))
