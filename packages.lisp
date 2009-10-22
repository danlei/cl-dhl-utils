;;;;;
;;;;; packages.lisp
;;;;;
;;;;; Time-stamp: <2009-10-22 16:30:01 danlei>
;;;;;


(in-package :common-lisp-user)

(defpackage #:dhl-utils
  (:documentation "Miscellaneous CL utilities.")
  (:nicknames #:dhlu)
  (:use :common-lisp)
  (:export #:mappend
	   #:count-atoms
	   #:count-everywhere
	   #:dot-product
	   #:cross-product
	   #:find-all-if
	   #:find-all
	   #:random-elt
	   #:with-gensyms
	   #:once-only
	   #:flatten
	   #:permute
	   #:print-list-as-sentence
	   #:ignore-warnings
	   #:straight-ahead
           #:deflex
  	   #:it
	   #:self
	   #:gather
	   #:awhen
	   #:aif
	   #:asetf
	   #:alet
	   #:awhile
	   #:aand
	   #:alambda
	   #:acond
	   #:bwhen
	   #:bif
	   #:bwhile
	   #:band
	   #:print-pathname
	   #:compare-pathnames
	   #:class-superclasses
	   #:specializer-methods
	   #:defsclass
           #:memoize-function
           #:memoize
           #:unmemoize
	   ))
