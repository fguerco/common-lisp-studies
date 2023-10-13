(defpackage #:l99
  (:use #:cl))

(in-package #:l99)

(load "utils.lisp")

;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; Arithmetic

(defun primep (n)
  "P31 (**) Determine whether a given integer number is prime"
  (cond
    ((<= n 3) (> n 1))
    ((or (evenp n) (zerop (mod n 3))) nil)
    (t (loop for x from 5 upto (1+ (isqrt n)) by 6
             never (or (zerop (mod n x)) (zerop (mod n (+ x 2))))))))
             
(defun euclidean-gcd (a b)
  "P32 (**) Determine the greatest common divisor of two positive integer numbers"
  (do ((a a b) (b b (mod a b)))
      ((zerop b) a)))

(defun coprimep (a b)
  "P33 (*) Determine whether two positive integer numbers are coprime"
  (= (euclidean-gcd a b) 1))

(defun totient-phi-naive (m)
  "P34 (**) Calculate Euler's totient function phi(m)"
  (if (= 1 m)
      1
      (loop for x from 1 to (1- m)
            counting (coprimep x m))))
