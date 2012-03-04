(in-package :cl-user)

(defpackage :dxh
  (:use :cl)
  (:export
   ;; HTML tags
   a b body br button caption code col dd dl dt div em form h1 h2 h3 head hr html i img input li link meta object ol option p pre script select span strong style sub sup table tbody td textarea thead th title tr ul
   ;; DXH-specific
   *styles*
   *xhtml-namespace*
   html-<xhc
   html->xhc
   html-list
   lis
   xhdoc)
  (:documentation "This program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2009-2012"))