;;;;;
;;;;; memoization.lisp
;;;;;
;;;;; Time-stamp: <2009-08-02 21:53:03 danlei>
;;;;;


(in-package #:dhl-utils)


(defun memoize-function (function)
  "Returns a memoized version of FUNCTION."
  (let ((values (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (value cachedp) (gethash args values)
        (if cachedp
            value
            (setf (gethash args values)
                  (apply function args)))))))

(defmacro memoize (function-name)
  "Turns FUNCTION-NAME into a memoized function."
  `(progn
     (setf (get ',function-name 'original-fdefinition)
           (symbol-function ',function-name))
     (setf (symbol-function ',function-name)
           (memoize-function (symbol-function ',function-name)))))

(defmacro unmemoize (function-name)
  "Restores FUNCTION-NAMEs original, non-memoized version."
  `(setf (symbol-function ',function-name)
         (get ',function-name 'original-fdefinition)))
