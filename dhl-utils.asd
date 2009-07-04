;;;;; -*- mode: common-lisp -*-
;;;;;
;;;;; dhl-utils.asd
;;;;;
;;;;; Time-stamp: <2009-07-04 20:47:27 danlei>
;;;;;


(in-package #:common-lisp-user)

(defpackage #:dhl-utils-system
  (:use #:common-lisp
	#:asdf))

(in-package #:dhl-utils-system)


(defsystem dhl-utils
  :name "dhl-utils"
  :version "0.2.0"
  :author "Daniel H. Leidisch"
  :license "LLGPL"
  :description "Miscellaneous CL utilities."
  :serial t
  :components ((:file "packages")
	       (:file "debug")
	       (:file "misc")
	       (:file "anaphora")
	       ))

(defmethod perform ((o load-op) (c (eql (find-system 'dhl-utils))))
  (pushnew :dhl-utils *features*))

(defmethod perform ((o test-op) (c (eql (find-system 'dhl-utils))))
  (operate 'load-op 'dhl-utils-test)
  (operate 'test-op 'dhl-utils-test :force t))
