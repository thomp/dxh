(in-package :cl-user)

(defpackage :dxh
  (:use :cl)
  (:export
   ;; HTML tags
   a b blockquote body br button caption code col dd del dl dt div em form h1 h2 h3 head hr html i img input label li link meta object ol option p pre script select span strong style sub sup table tbody td textarea thead th title tr u ul
   ;; HTML tags (string output)
   a-s b-s blockquote-s body-s br-s button-s caption-s code-s col-s dd-s del-s dl-s dt-s div-s em-s form-s h1-s h2-s h3-s head-s hr-s html-s i-s img-s input-s label-s li-s link-s meta-s object-s ol-s option-s p-s pre-s script-s select-s span-s strong-s style-s sub-s sup-s table-s tbody-s td-s textarea-s thead-s th-s title-s tr-s u-s ul-s

   ;; DXH-specific
   *xhtml-namespace*
   html-<xhc
   html->xhc
   html-list
   lis
   xhdoc)
  (:documentation "This program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2009-2016"))
