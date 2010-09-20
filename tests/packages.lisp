;;;;;
;;;;; packages.lisp
;;;;;
;;;;; Time-stamp: <2010-09-20 12:32:31 danlei>
;;;;;


(in-package #:common-lisp-user)

(defpackage #:dhl-utils-test
  (:documentation "Test package for dhl-utils.")
  (:use #:common-lisp
        #:dhl-utils
        #:lisp-unit)
  (:export #:do-tests))
