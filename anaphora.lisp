;;;;;
;;;;; anaphora.lisp
;;;;;
;;;;; Time-stamp: <2009-08-07 22:26:41 danlei>
;;;;;
;;;;; Anaphoric macros in the style of Paul Graham's "On Lisp" (ch. 14).
;;;;;


(in-package #:dhl-utils)


(defmacro awhen (expression &body body)
  "Anaphoric WHEN: Implicitly binds IT to EXPRESSION."
  `(let ((it ,expression))
     (when it ,@body)))

(defmacro aif (expression then &optional else)
  "Anaphoric IF: Implicitly binds IT to EXPRESSION."
  `(let ((it ,expression))
     (if it ,then ,else)))

(defmacro asetf (place &body body)
  "Anaphoric SETF: Binds IT to PLACE."
  `(let ((it ,place))
     (setf ,place ,@body)))

(defmacro alet (expression &body body)
  "Anaphoric LET shortcut: Binds IT to EXPRESSION."
  `(let ((it ,expression))
     ,@body))

(defmacro awhile (expression &body body)
  "Anaphoric WHILE: Executes BODY while EXPRESSION,
which is bound to IT, is true. GATHER may be called
within BODY to collect values in a list, which is
finally returned."
  (let ((result (gensym "RESULT-")))
    `(let ((,result (list)))
       (flet ((gather (item)
		(push item ,result)))
	 (do ((it ,expression ,expression))
	     ((not it) (nreverse ,result))
	   ,@body)))))

(defmacro aand (&rest args)
  "Anaphoric AND: IT is implicitly bound to the preceding argument."
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(awhen ,(car args) (aand ,@(cdr args))))))

(defmacro alambda (parameters &body body)
  "Anaphoric LAMBDA: Binds SELF implicitly to itself."
  `(labels ((self ,parameters ,@body))
     #'self))

(defmacro acond (&rest clauses)
  "Anaphoric COND: Binds IT to the car of the corresponding clause."
  (if (null clauses)
      nil
      (let ((clause (car clauses)))
	`(let ((it ,(car clause)))
	   (if it
	       (progn ,@(cdr clause))
	       (acond ,@(cdr clauses)))))))

(defmacro bwhen ((var expression) &body body)
  "Binding WHEN: Binds VAR to EXPRESSION."
  `(let ((,var ,expression))
     (when ,var)
     ,@body))

(defmacro bif ((var expression) then &optional else)
  "Binding IF: Binds VAR to EXPRESSION."
  `(let ((,var ,expression))
     (if ,var ,then ,else)))

(defmacro bwhile ((var expression) &body body)
  "Binding WHILE: Executes BODY while EXPRESSION,
which is bound to VAR, is true. GATHER may be called
within BODY to collect values in a list, which is
finally returned."
  (let ((result (gensym "RESULT-")))
    `(let ((,result (list)))
       (flet ((gather (item)
		(push item ,result)))
       (do ((,var ,expression ,expression))
	   ((not ,var) (nreverse ,result))
	 ,@body)))))

(defmacro band ((var) &rest args)
  "Binding AND: VAR is implicitly bound to the preceding argument."
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(bif (,var ,(car args)) (band (,var) ,@(cdr args))))))
