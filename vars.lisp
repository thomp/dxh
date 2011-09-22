(in-package :dxh)
;;;
;;; vars.lisp: variable definitions for DXH
;;;
(defvar *default-head-content* 
  nil ; 'default-head-content-xhc
  "Symbol representing function which should be called to generate default content in HEAD component."
  )

;; test:
;; (let ((dxh:*xhtml-namespace* nil)) (dxh:html-xhc "joejoe"))
(defvar *xhtml-namespace*
  "" ; "h"
  "Either NIL or a string representing the namespace for xhtml content. For example, 'h' would indicate that <p> should be rendered in an xhtml document as <h:p>.")




