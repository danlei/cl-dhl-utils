;;;;;
;;;;; misc-tests.lisp
;;;;;
;;;;; Time-stamp: <2010-02-11 23:01:32 danlei>
;;;;;


(in-package #:dhl-utils-test)


(defun do-tests ()
  (run-all-tests :dhl-utils-test))


(define-test mappend
  (assert-equal '(1 3 5)
		(mappend (lambda (x)
			   (list (car x)))
			 '((1 2) (3 4) (5 6))))
  (assert-equal '((1 2 3) (3 2 1) (a b c) (c b a))
		(mappend (lambda (list)
			   (list list (reverse list)))
			 '((1 2 3) (a b c))))
  (assert-equal '(1 2 3 4 5 6 7 8)
		(mappend #'append '((1 2) (5 6)) '((3 4) (7 8))))
  (assert-equal '((1 2) (3 4) (5 6) (7 8))
		(mappend #'list '((1 2) (5 6)) '((3 4) (7 8))))
  (assert-error 'error (mappend))
  (assert-error 'error (mappend t))
  (assert-error 'error (mappend t t)))

(define-test count-atoms
  (assert-equal 0 (count-atoms '(())))
  (assert-equal 4 (count-atoms '(a (b c) () d)))
  (assert-error 'error (count-atoms t))
  (assert-error 'error (count-atoms)))

(define-test count-everywhere
  (assert-equal 0 (count-everywhere 'a nil))
  (assert-equal 0 (count-everywhere 'a '(b c d (e f (g)))))
  (assert-equal 3 (count-everywhere 'a '((a x 1) b d a ((i d) (a)))))
  (assert-equal 2 (count-everywhere '(a b) '((a b) (c d) ((a b)))))
  (assert-equal 1 (count-everywhere 'a 'a))
  (assert-error 'error (count-everywhere))
  (assert-error 'error (count-everywhere t)))

(define-test dot-product
  (assert-equal 0 (dot-product '(0) '(0)))
  (assert-equal 0 (dot-product '(0) '(1)))
  (assert-equal 2 (dot-product '(1 1) '(1 1)))
  (assert-equal 3 (dot-product '(1 1) '(2 1)))
  (assert-equal 4 (dot-product '(1 1) '(2 2)))
  (assert-error 'error (dot-product))
  (assert-error 'error (dot-product t t)))

(define-test cross-product
  (assert-equal () (cross-product #'* '() '()))
  (assert-equal '((1 a) (2 a) (1 b) (2 b))
		(cross-product #'list '(1 2) '(a b)))
  (assert-error 'error (cross-product))
  (assert-error 'error (cross-product t t t)))

(define-test find-all
  (assert-equal '() (find-all 'a '(b c d e)))
  (assert-equal '((a b) (a 4)) (find-all 'a
					 '((a b) (d 2) (d 3) (a 4) (z 9))
					 :test (lambda (x y) (eq x (car y)))))
  (assert-equal '(b c d) (find-all 'a '(a b a c d a) :test-not #'eq))
  (assert-error 'error (find-all))
  (assert-error 'error (find-all t))
  (assert-error 'error (find-all t t)))

(define-test random-elt
  (assert-equal '() (random-elt '()))
  (assert-equal 'a (random-elt '(a)))
  (assert-error 'error (random-elt t))
  (assert-error 'error (random-elt)))

(define-test with-gensyms
  (assert-expands '(let ())
		  (with-gensyms ()))
  (assert-expands '(let ((a (gensym))))
		  (with-gensyms (a)))
  (assert-expands '(let ((a (gensym))
			 (b (gensym))))
		  (with-gensyms (a b)))
  (assert-expands '(let ((a (gensym))
			 (b (gensym)))
		    a)
		  (with-gensyms (a b) a)))

(define-test flatten
  (assert-equal '() (flatten '()))
  (assert-equal '(a a a a) (flatten '((a) (a) (a) (a))))
  (assert-equal '(a b c d e) (flatten '((a) (b (c ()) d (e)))))
  (assert-equal '(t) (flatten t))
  (assert-error 'error (flatten)))

(define-test permute
  (assert-equal '() (permute '()))
  (assert-equal 4 (length (permute '(1 2 3 4))))
  (assert-equal '(a a a) (permute '(a a a)))
  (assert-error 'error (permute))
  (assert-error 'error (permute t)))

(define-test permutations
  (assert-equal '() (permutations '()))
  (assert-equal '((1)) (permutations '(1)))
  (assert-equal '((1 2) (2 1)) (permutations '(1 2)))
  (assert-equal 12 (length (permutations '(1 2 3 4))))
  (assert-error 'error (permutations))
  (assert-error 'error (permutations t)))

(define-test print-list-as-sentence
  (assert-prints "" (print-list-as-sentence '()))
  (assert-prints "The." (print-list-as-sentence '(the)))
  (assert-prints "The quick brown fox." (print-list-as-sentence
					 '(the quick brown fox)))
  (assert-error 'error (print-list-as-sentence))
  (assert-error 'error (print-list-as-sentence t)))

(define-test ignore-warnings
  (assert-true (ignore-warnings () t))
  (assert-true (ignore-warnings () (warn "") t))
  (assert-false (ignore-warnings () nil))
  (assert-false (ignore-warnings () (warn "") nil))
  (assert-prints "" (ignore-warnings () (warn 'warning)))
  (assert-error 'error (ignore-warnings)))

(define-test straight-ahead
  (assert-true (straight-ahead () (cerror "" 'condition) t))
  (assert-false (straight-ahead () (cerror "" 'condition) nil))
  (assert-error 'error (straight-ahead)))

(define-test awhen
  (assert-false (awhen t))
  (assert-false (awhen nil))
  (assert-false (awhen 1))
  (assert-equal 1 (awhen 1 it))
  (assert-equal 2 (awhen 1 (* 2 it))))

(define-test aif
  (assert-true (aif t t))
  (assert-false (aif nil t))
  (assert-true (aif nil t t))
  (assert-false (aif nil t nil))
  (assert-true (aif t it))
  (assert-equal 1 (aif 1 it))
  (assert-equal 5 (aif nil nil 5))
  (assert-equal 10 (aif 5 (* 2 it))))

(define-test asetf
  (assert-true (let ((x t))
		 (asetf x it)
		 x))
  (assert-equal 2 (let ((x 1))
		    (asetf x (* 2 it))
		    x))
  (assert-equal 4 (let ((v (vector 2)))
		    (asetf (aref v 0) (* 2 it))
		    (aref v 0))))

(define-test alet
  (assert-true (alet t it))
  (assert-false (alet t))
  (assert-equal 1 (alet 1 it))
  (assert-equal 4 (alet 2 (* 2 it))))

(define-test bwhen
  (assert-false (bwhen (it t)))
  (assert-false (bwhen (it nil)))
  (assert-false (bwhen (it 1)))
  (assert-equal 1 (bwhen (it 1) it))
  (assert-equal 2 (bwhen (it 1) (* 2 it))))

(define-test bif
  (assert-true (bif (it t) t))
  (assert-false (bif (it nil) t))
  (assert-true (bif (it nil) t t))
  (assert-false (bif (it nil) t nil))
  (assert-true (bif (it t) it))
  (assert-equal 1 (bif (it 1) it))
  (assert-equal 5 (bif (it nil) nil 5))
  (assert-equal 10 (bif (it 5) (* 2 it))))

#+nil
(define-test compare-pathnames
  #+(or unix linux)
  (assert-prints "" (compare-pathnames
		     #p"/home/danlei/test.lisp"
		     (make-pathname :directory '(:absolute "home" "danlei")
				    :name "test"
				    :type "lisp"
				    :version :newest)))
  #+(or unix linux)
  (assert-prints "" (compare-pathnames
		     #p"code/README"
		     (make-pathname :directory '(:relative "code")
				    :name "README"
				    :version :newest)))
  (assert-prints "" (compare-pathnames #p"/home/danlei/test.lisp"
				       #p"/home/danlei/test.lisp"))
  (assert-error 'error (compare-pathnames #p"x")))

(define-test ignore-warnings
  (assert-prints "" (ignore-warnings ()
		      (warn 'warning)))
  (assert-error 'error (ignore-warnings ()
			 (error 'error))))

(define-test straight-ahead
  (assert-true (straight-ahead ()
		 (progn
		   (cerror "" 'error)
		   t)))
  (assert-error 'error (straight-ahead (warning)
			 (cerror "" 'error)))
  (assert-error 'error (straight-ahead ()
			 (error 'error))))

(define-test deflex
  (assert-equal '(1 2)
                (progn
                  (deflex x 2)
                  (list (let ((x 1))
                          ((lambda () x)))
                        x)))
  (assert-equal '(1 2)
                (progn
                  (deflex x)
                  (setq x 2)
                  (list (let ((x 1))
                          ((lambda () x)))
                        x))))
