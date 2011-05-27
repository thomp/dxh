;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(defpackage :dxh
  (:use :cl)
  (:export
   *default-xh-top*
   *styles*
   *xhtml-namespace*
   a-xhc
   A-XHCS
   alpha-links-for-list-xhc
   b-xhc
   body-xhc
   br-xhc
   button-xhc
   column-divs
   "COLUMNS-VALUES-XHTML"
   "COLUMNS-VALUES-XHTML-NO-TABLE" 
   column-widths
   "DEFAULT-XH-TOP"
   "DEFAULT-XH-TOP-CONTENT" 
   div-xhc
   error-xh
   error-xhc
   form-xhc
   h1-xhc
   h2-xhc
   h3-xhc
   head-xhc
   hr-xhc
   html-list
   "HTML-TABLE"
   "HTML-TABLE-COLS" "HTML-TABLE-ROW"
   "HTML-TABLE-ROWS"
   html-xhc
   html-<xhc
   html->xhc
   img-xhc
   input-xhc
   li-xhc
   li-xhcs
   link-xhc
   matching-tags-p  
   NON-HTML-CHARS-P
   ol-xhc
   option-xhc
   p-xhc
   pre-xhc
   script-xhc
   select-xhc
   span-xhc
   style-xhc
   sub-xhc
   sup-xhc
   table-column-widths
   table-xhc
   tbody-xhc
   td-xhc
   textarea-xhc
   thead-xhc
   th-xhc
   tr-xhc
   ul-xhc
   "XH-COLGROUP"
   xhdoc)
  (:documentation "Keep-it-simple (macro-free) HTML markup. This program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2009,2010")
  )