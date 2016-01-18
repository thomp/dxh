(in-package :dxh)

;; FIXME: add INPUT and LABEL

(defun a (output-generator stream &key attributes class id href rwname rwval style target title)
  (let ((newhref
	 (if (and rwname rwval)
	     (url-rewrite:add-get-param-to-url href rwname rwval)
	     href))) 
    (cond ((stringp output-generator) 
	   (xhc* "a" output-generator
		 :attributes (append attributes
				     (list  
				      (list "href" newhref) 
				      (list "target" target) 
				      (list "title" title)))
		 :class class
		 :id id
		 :style style
		 :stream stream))
	  (t (error "Write me")))))

(defun img/ (stream &key (alt "ALT not specified.") class src style title)
  (dxg::empty-tag* "img" 
		   stream
		   :attributes (list
				(list "alt" alt)
				(list "class" class)
				(list "src" src)
				(list "style" style)
				(list "title" title))))

(defun input (stream &key accesskey checked class id onblur onfocus readonly style type name tabindex title value)  
  (xhc* "input"
	""
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

(defun label (output-generator stream &key class for style)
  (cond ((stringp output-generator) 
	 (xhc* "label"
	       output-generator
	       :attributes (list (list "for" for)) 
	       :class class 
	       :stream stream
	       :style style))
	(t (error "Write me"))))

(defun meta/ (stream &key (content "text/html;charset=utf-8") (http-equiv "Content-Type") name scheme)
  (dxg::empty-tag* "meta" stream
		   :attributes (list
				(list "content" content)
				(list "http-equiv" http-equiv)
				(list "name" name)
				(list "scheme" scheme))))

(defun option (output-generator stream &key class label selected style title value)
  (cond ((stringp output-generator) 
	 (xhc* "option"
	       output-generator
	       :attributes (list 
			    (list "label" label)
			    (list "value" value)
			    (list "selected" selected)
			    (list "title" title)) 
	       :class class 
	       :stream stream
	       :style style))
	(t (error "Write me"))))

(defun style (output-generator stream &key protect-with-cdata-p (type "text/css"))
  "TYPE is a string representing the type attribute."
  (cond ((stringp output-generator) 
	 (xhc* "style"
	       (if protect-with-cdata-p
		   ;; CDATA needed with some browsers (e.g., firefox 3.5)
		   (concatenate 'string "/* <![CDATA[ */
"
				output-generator
				"
/* ]]> */")
		   output-generator) 
	       :attributes (list (list "type" type))
	       :stream stream))
	(t (error "Write me"))))
