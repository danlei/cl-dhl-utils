;;;;;
;;;;; clos.lisp
;;;;;
;;;;; Time-stamp: <2010-09-20 12:20:24 danlei>
;;;;;


(in-package #:dhl-utils)


#+closer-mop
(defun class-superclasses (class)
  "Returns a list of all supperclasses of CLASS."
  (let ((direct-superclasses (closer-mop:class-direct-superclasses class)))
    (if (endp direct-superclasses)
        nil
        (append direct-superclasses
                (mappend #'class-superclasses
                         direct-superclasses)))))

#-closer-mop
(progn
  (warn "CLASS-SUPERCLASSES depends on CLOSER-MOP.")
  (defun class-superclasses (class)
    (declare (ignorable class))
    (error "CLASS-SUPERCLASSES depends on CLOSER-MOP.")))

#+closer-mop
(defun specializer-methods (specializer)
  "Returns a list of all methods that specialize
on SPECIALIZER or one of its superclasses."
  (mappend #'closer-mop:specializer-direct-methods
           (cons specializer (class-superclasses specializer))))

#-closer-mop
(progn
  (warn "SPECIALIZER-METHODS depends on CLOSER-MOP.")
  (defun specializer-methods (specializer)
    (declare (ignorable specializer))
    (error "SPECIALIZER-METHODS depends on CLOSER-MOP.")))

(defun generate-slot-options (slot)
  "Returns a list of slot-options for SLOT."
  `(,slot :accessor ,slot
          :initarg ,(intern (symbol-name slot) 'keyword)))

(defmacro defsclass (name super-classes &rest slots)
  "Defines a class named NAME inheriting from SUPER-CLASSES,
generates accessors and initargs for each of SLOTS (with the
same name as the according slot), and creates a constructor
funtion named MAKE-NAME."
  `(progn
     (setf (symbol-function ',(intern (concatenate 'string "MAKE-"
                                                   (symbol-name name))))
           (lambda (&rest args) (apply #'make-instance ',name args)))
     (defclass ,name ,super-classes
       ,(mapcar #'generate-slot-options slots))))
