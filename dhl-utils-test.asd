;;;;; -*- mode: common-lisp -*-
;;;;;
;;;;; dhl-utils-test.asd
;;;;;
;;;;; Time-stamp: <2009-04-25 23:57:41>
;;;;;


(in-package #:common-lisp-user)

(defpackage #:dhl-utils-test-system
  (:use #:common-lisp
	#:asdf))

(in-package #:dhl-utils-test-system)


(defsystem dhl-utils-test
  :name "dhl-utils-test"
  :version "0.2.0"
  :author "Daniel H. Leidisch"
  :license "LLGPL"
  :description "Test suite for dhl-utils."
  :depends-on (dhl-utils)
  :components ((:module tests
			:serial t
			:components ((:file "packages")
				     (:file "cl-unit")
				     (:file "misc-tests")
				     ))))

(defmethod perform ((o test-op) (c (eql (find-system 'dhl-utils-test))))
  (operate 'load-op 'dhl-utils-test)
  (funcall (intern (symbol-name '#:do-tests)
		   (find-package '#:dhl-utils-test))))
