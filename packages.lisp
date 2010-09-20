;;;;;
;;;;; packages.lisp
;;;;;
;;;;; Time-stamp: <2010-09-20 12:14:11 danlei>
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
           #:permutations
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
