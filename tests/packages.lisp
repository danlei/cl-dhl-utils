;;;;;
;;;;; packages.lisp
;;;;;
;;;;; Time-stamp: <2009-04-25 23:56:00>
;;;;;


(in-package #:common-lisp-user)

(defpackage #:lisp-unit
  (:use #:common-lisp)
  (:export #:define-test
	   #:run-all-tests
	   #:run-tests
           #:assert-eq
	   #:assert-eql
	   #:assert-equal
	   #:assert-equalp
           #:assert-error
	   #:assert-expands
	   #:assert-false
           #:assert-equality
	   #:assert-prints
	   #:assert-true
           #:get-test-code
	   #:get-tests
           #:remove-all-tests
	   #:remove-tests
           #:logically-equal
	   #:set-equal
           #:use-debugger
           #:with-test-listener))

(defpackage #:dhl-utils-test
  (:documentation "Test package for dhl-utils.")
  (:use #:common-lisp
	#:dhl-utils
	#:lisp-unit)
  (:export #:do-tests))
