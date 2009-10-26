;;;;;
;;;;; misc.lisp
;;;;;
;;;;; Time-stamp: <2009-10-26 00:29:45 danlei>
;;;;;


(in-package #:dhl-utils)


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

(defmacro ignore-warnings ((&optional (condition 'warning)) &body body)
  "Tacitly ignores warnings of type CONDITION within BODY."
  `(handler-bind ((,condition #'muffle-warning))
     ,@body))

(defmacro straight-ahead ((&optional (condition 'condition)
                                     (restart 'continue))
                          &body body)
  "Tries to invoke CONTINUE restarts for CONDITION, if
the latter is signaled during the execution of BODY."
  `(handler-bind ((,condition (lambda (c)
                                (declare (ignore c))
                                (invoke-restart ',restart))))
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

(defmacro deflex (var &optional value docstring)
  "Establishes a toplevel lexical variable named VAR,
if given, assigns VALUE to it, and attaches DOCSTRING."
  `(progn
     (define-symbol-macro ,var (get ',var 'lexical-value))
     (setf ,var ,value)
     (setf (get ',var 'lexical-documentation) ',docstring)
     ',var))
