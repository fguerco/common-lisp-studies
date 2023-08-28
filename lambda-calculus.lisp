;; Fun with lambda calculus
;; https://web.archive.org/web/20230827210328/https://stopa.io/post/263

(defpackage :lambda-calculus
   (:use :cl))

(in-package :lambda-calculus)

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

(defmacro call (fn &rest args)
  `(funcall ,fn ,@args))

(defmacro def (name value)
  `(defparameter ,name ,value))

;; pair
(def pair (fn (a b) (fn (f) (call f a b))))
(def car. (fn (f) (call f (fn (a b) (declare (ignore b)) a))))
(def cdr. (fn (f) (call f (fn (a b) (declare (ignore a)) b))))

;; numbers
(def zero (fn (f v) (declare (ignore f)) v))
(def one (fn (f v) (call f (call zero f v))))
(def two (fn (f v) (call f (call one f v))))

;; booleans
(def true (fn (tv fv) (declare (ignore fv)) tv))
(def false (fn (tv fv) (declare (ignore tv)) fv))

;; conversion functions for testing
(defun lc->int (lc)
  (call lc #'1+ 0))

(defun int->lc (int)
  (cond
    ((zerop int) zero)
    (t (call incr (int->lc (1- int))))))

(defun truep (lc-bool)
  (call lc-bool t nil))

(def incr
    (fn (lc)
      (fn (f v) (call f (call lc f v)))))

(def shift-and-inc
    (fn (p)
      (call pair (call cdr. p)
                 (call incr (call cdr. p)))))

(def decr
  (fn (lc)
    (call car.
          (call lc shift-and-inc (call pair zero zero)))))
          
(def times
    (fn (lc1 lc2)
      (fn (f v)
        (call lc2 (fn (x) (call lc1 f x)) v))))

(def if.
  (fn (bool then else)
    (call bool then else)))

(def zero?
  (fn (lcn)
    (call lcn (fn (v) (declare (ignore v)) false)
              true)))

;; now factorial with bound variable for recursion
(def factorial-v0
  (fn (lcn)
    (call if. (call zero? lcn)
          (fn () one)
          (fn ()
            (call times lcn (call (call factorial-v0 (call decr lcn))))))))

;; and next call recursion only with anonymous functions
;; this is incomplete - move up the recursive function
(call
 (call
  (fn (lcn)
    (call
     (fn (rec lcn)
       (call rec rec lcn))
     (fn (self lcn)
       (call if. (call zero? lcn)
             (fn () one)
             (fn ()
               (call times lcn (call (call self self (call decr lcn)))))))
     lcn))
  (int->lc 5)))


;; now full Y combinator
(call (call (fn (f)
              (call (fn (x) (call x x))
                    (fn (x)
                      (call f (fn (a)
                                (call (call x x) a))))))
            (fn (f)
              (fn (n)
                (call if. (call zero? n)
                      (fn () one)
                      (fn ()
                        (call times n (call (call f (call decr n)))))))))
      (int->lc 5))
