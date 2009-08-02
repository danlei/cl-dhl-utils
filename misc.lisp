;;;;;
;;;;; misc.lisp
;;;;;
;;;;; Time-stamp: <2009-08-02 21:20:40 danlei>
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

(defmacro with-gensyms ((&rest names) &body body)
  "Initializes variables in NAMES with gensymed symbols."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

#+nil
(defmacro once-only ((&rest names) &body body)
  "Evaluates macro arguments only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro once-only (variables &rest body)
  "Evaluates macro arguments only once."
  (let ((temps nil))
    (dotimes (i (length variables)) (push (gensym) temps))
    ``(let (,,@(mapcar (lambda (tmp var) ``(,',tmp ,,var))
		       temps variables))
	,(let ,(mapcar (lambda (var tmp) `(,var ',tmp))
		       variables temps)
	      .,body))))

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

(defmacro ignore-warnings ((&optional (condition 'warning)) &body body)
  "Tacitly ignores warnings of type CONDITION within BODY."
  `(handler-bind ((,condition #'muffle-warning))
     ,@body))

(defmacro straight-ahead ((&optional (condition 'condition)) &body body)
  "Tries to invoke CONTINUE restarts for CONDITION, if
the latter is signaled during the execution of BODY."
  `(handler-bind ((,condition #'continue))
     ,@body))

(defun print-pathname (pathname)
  "Prints an overview of PATHNAME."
  ;; P. Bourguignon
  (format t "~a~%" pathname)
  (format t "~&~{~{~@(~9A~) : ~S~&~}~}"
	  (mapcar (lambda (name field) (list name (funcall field pathname)))
		  '(host device directory name type version)
		  '(pathname-host pathname-device pathname-directory
		    pathname-name pathname-type pathname-version))))

(defun compare-pathnames (p1 p2)
  "Compares the pathnames P1 and P2, then gives a printed overview"
  ;; P. Bourguignon
  (flet ((compare (name field)
	   (unless (equal (funcall field p1) (funcall field p2))
	     (format t "~&~A DIFFERENT: ~A /= ~A~%"
		     name (funcall field p1) (funcall field p2)))))
    (compare 'host      (function pathname-host))
    (compare 'device    (function pathname-device))
    (compare 'directory (function pathname-directory))
    (compare 'name      (function pathname-name))
    (compare 'type      (function pathname-type))
    (compare 'version   (function pathname-version))))

#+closer-mop
(defun class-superclasses (class)
  "Returns a list of all supperclasses of CLASS."
  (let ((direct-superclasses (closer-mop:class-direct-superclasses class)))
    (if (endp direct-superclasses)
	nil
	(append direct-superclasses
		(mappend #'class-superclasses
			 direct-superclasses)))))
