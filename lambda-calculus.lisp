;; Fun with lambda calculus
;; https://web.archive.org/web/20230827210328/https://stopa.io/post/263

(defpackage :lambda-calculus
   (:use :cl))

(in-package :lambda-calculus)

(defmacro def (name value)
  `(defparameter ,name ,value))

;; pair
(def pair (lambda (a b) (lambda (f) (funcall f a b))))
(def car. (lambda (f) (funcall f (lambda (a b) (declare (ignore b)) a))))
(def cdr. (lambda (f) (funcall f (lambda (a b) (declare (ignore a)) b))))

;; numbers
(def zero (lambda (f v) (declare (ignore f)) v))
(def one (lambda (f v) (funcall f (funcall zero f v))))
(def two (lambda (f v) (funcall f (funcall one f v))))

;; booleans
(def true (lambda (tv fv) (declare (ignore fv)) tv))
(def false (lambda (tv fv) (declare (ignore tv)) fv))

;; conversion functions for testing
(defun lc->int (lc)
  (funcall lc #'1+ 0))

(defun int->lc (int)
  (cond
    ((zerop int) zero)
    (t (funcall incr (int->lc (1- int))))))

(defun truep (lc-bool)
  (funcall lc-bool t nil))

(def incr
    (lambda (lc)
      (lambda (f v) (funcall f (funcall lc f v)))))

(def shift-and-inc
    (lambda (p)
      (funcall pair (funcall cdr. p)
                 (funcall incr (funcall cdr. p)))))

(def decr
  (lambda (lc)
    (funcall car.
          (funcall lc shift-and-inc (funcall pair zero zero)))))
          
(def times
    (lambda (lc1 lc2)
      (lambda (f v)
        (funcall lc2 (lambda (x) (funcall lc1 f x)) v))))

(def if.
  (lambda (bool then else)
    (funcall bool then else)))

(def zero?
  (lambda (lcn)
    (funcall lcn (lambda (v) (declare (ignore v)) false)
              true)))

;; now factorial with bound variable for recursion
(def factorial-v0
  (lambda (lcn)
    (funcall if. (funcall zero? lcn)
          (lambda () one)
          (lambda ()
            (funcall times lcn (funcall (funcall factorial-v0 (funcall decr lcn))))))))

;; and next call recursion only with anonymous functions
;; this is incomplete - move up the recursive function
(funcall
 (funcall
  (lambda (lcn)
    (funcall
     (lambda (rec lcn)
       (funcall rec rec lcn))
     (lambda (self lcn)
       (funcall if. (funcall zero? lcn)
             (lambda () one)
             (lambda ()
               (funcall times lcn (funcall (funcall self self (funcall decr lcn)))))))
     lcn))
  (int->lc 5)))


;; now full Y combinator (z combinator to be precise)
(funcall
 (funcall
  (lambda (f)
    (funcall (lambda (x) (funcall x x))
          (lambda (x)
            (funcall f (lambda (a)
                      (funcall (funcall x x) a))))))
  (lambda (f)
    (lambda (n)
      (funcall if. (funcall zero? n)
            (lambda () one)
            (lambda ()
              (funcall times n (funcall (funcall f (funcall decr n)))))))))
 (int->lc 5))

;; we can check the result calling
;; (lc->int (funcall <above code>))
;; this call is needed because this if evaluates parameters so we pass in
;; lambdas in order to do lazy evaluation
