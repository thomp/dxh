(in-package :dxh)

;; elements which should have definitions automatically generated
(defparameter *auto-elements* 
  '(b blockquote body br 
    caption code 
    del div dd dl dt 
    em 
    h1 h2 h3 hr 
    i 
    li 
    ol 
    p pre 
    span strong sub sup 
    table tbody td th thead title tr 
    u ul))

;; unresolved: more natural/idiomatic to use a string for tag or a symbol for tag (defelt 'foo ...) vs. (defelt "foo" ...) ?

(defun defelt (tag)
  "Return value undefined. TAG is a symbol. Define four global functions, one associated with the symbol TAG, one associated with the symbol corresponding to TAG-S, one associated with the symbol corresponding to TAG+, and one associated with the symbol corresponding to TAG/. TAG accepts, as arguments, a string or a funcallable object along with a stream. TAG-S returns a string (and does not interact with an externally supplied output stream); TAG+ accepts a funcallable object which accepts a stream as an argument; TAG/ generates an empty element."
  (let ((tag-string (string-downcase (string tag))))
    (setf (fdefinition tag)
	  #'(lambda (output-generator stream &key attributes class id style)
	      (format t "DEFELT.22 OUTPUT-GENERATOR(~S): ~S~%" tag output-generator)
	      (cond ((not output-generator)
		     (dxg::empty-tag* 
		      tag stream 
		      :attributes (append (list 
					   (list "class" class)
					   (list "style" style)
					   (list "id" id))
					  attributes)))
		    ((stringp output-generator)
		     (format t "DEFELT.33")
		     (xhc* tag-string output-generator :attributes attributes :class class :id :id :style style :stream stream))
		    (t
		     (xhc+ tag-string output-generator stream
		     :attributes attributes 
		     :class class
		     :id id 
		     :style style)))))

    ;; FOO-S
    (let ((tag-s-symbol (read-from-string (concatenate 'string tag-string "-s"))))
     (setf (fdefinition tag-s-symbol)
	   #'(lambda (some-string &key attributes class id style)
	       (xhc tag-string some-string 
		    :attributes attributes 
		    :class class
		    :id id
		    :style style)))) 
    ;; FOO+
   ;; use read to avoid readtable case issues
   (let ((tag+-symbol (read-from-string (concatenate 'string tag-string "+"))))
     (setf (fdefinition tag+-symbol) 
	   #'(lambda (f stream &key attributes class id style)
	       (xhc+ tag-string f stream
		     :attributes attributes 
		     :class class
		     :id id 
		     :style style))))
   ;; FOO/
   (let ((tag/-symbol (read-from-string (concatenate 'string tag-string "/"))))
     (setf (fdefinition tag/-symbol) 
	   #'(lambda (stream &key attributes class id style)
	       (dxg::empty-tag* tag-string stream
		     :attributes (append (list 
					   (list "class" class)
					   (list "style" style)
					   (list "id" id))
					  attributes)))))))

(map nil #'(lambda (symbol)
	     (defelt symbol))
     *auto-elements*)