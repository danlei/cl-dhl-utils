;;;;;
;;;;; packages.lisp
;;;;;
;;;;; Time-stamp: <2009-08-02 06:23:05 danlei>
;;;;;


(in-package #:common-lisp-user)

(defpackage #:dhl-utils-test
  (:documentation "Test package for dhl-utils.")
  (:use #:common-lisp
	#:dhl-utils
	#:lisp-unit)
  (:export #:do-tests))
