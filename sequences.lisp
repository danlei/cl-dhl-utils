;;;;;
;;;;; sequences.lisp
;;;;;
;;;;; Time-stamp: <2009-08-02 21:31:25 danlei>
;;;;;


(in-package #:dhl-utils)


(defun mappend (function &rest lists)
  "Appends the results of mapping FUNCTION over LISTS."
  (apply #'append (apply #'mapcar function lists)))

(defun count-atoms (list)
  "Counts all non-nil atoms within LIST."
  (check-type list list)
  (cond ((endp list) 0)
	((listp (car list))
	 (+ (count-atoms (car list))
	    (count-atoms (cdr list))))
	(t (1+ (count-atoms (cdr list))))))

(defun count-everywhere (item expression)
  "Counts all occurrences of ITEM within EXPRESSION."
  (cond ((equal item expression) 1)
	((atom expression) 0)
	(t (+ (count-everywhere item (car expression))
	      (count-everywhere item (cdr expression))))))

(defun dot-product (list1 list2)
  "Computes the mathematical dot product of two lists."
  (apply #'+ (mapcar #'* list1 list2)))

(defun cross-product (function list1 list2)
  "Calls FUNCTION with all elements of LIST1
and LIST2 paired, then appends the results."
  (mappend (lambda (y)
	     (mapcar (lambda (x) (funcall function x y))
		     list1))
	   list2))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  "Finds all elements of SEQUENCE that match ITEM,
according to the keywords."
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(defun random-elt (sequence)
  "Returns a random element of SEQUENCE."
  (if sequence
      (elt sequence (random (length sequence)))
      nil))

(defun flatten (tree)
  "Returns a list of all leaves of TREE."
  (labels ((rec (tree acc)
	     (cond ((null tree) acc)
		   ((atom tree) (cons tree acc))
		   (t (rec
		       (car tree)
		       (rec (cdr tree) acc))))))
    (rec tree nil)))
    
(defun permute (list)
  "Returns a permutation of LIST."
  (if (endp list)
      nil
      (let ((item (random-elt list)))
	(cons item (permute (remove item list :count 1))))))

(defun print-list-as-sentence (list)
  "Prints LIST as an english sentence, with the first
word capitalized, and a dot at the end."
  (check-type list list)
  (format t "~@[~@(~{~a~^ ~}~).~]" list)
  "")