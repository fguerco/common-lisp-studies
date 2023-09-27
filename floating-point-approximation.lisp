(ql:quickload :parachute)

(defpackage :fp-approx
  (:use :cl :parachute))

(in-package :fp-approx)


(defun trunc (x) (/ (floor (* x 100.0)) 100.0))

(defun interp (fn lower upper n)
  (loop with d = (/ (- upper lower) n)
        for i from 0 below n
        for x = (+ lower (* i d))
        collect (trunc (funcall fn x))))

(define-test test-identity
  (is equalp '(0.0 2.25 4.5 6.75) (interp #'identity 0.0 9.0 4))
  (is equalp '(0.0 1.66 3.33 5.0 6.66 8.33 10.0 11.66 13.33)
      (interp #'identity 0.0 15.0 9)))

(define-test test-sin
  (is equalp  '(0.0 0.99 0.14 -0.98 -0.28 0.93 0.41 -0.88 -0.54 0.8 0.65 -0.72)
      (interp #'sin 0 18.0 12))
  (is equalp '(0.0 0.86 -0.88 0.01 0.85 -0.88 0.03 0.84 -0.89 0.05)
      (interp #'sin 0 21.0 10)))

(define-test test-cos
  (is equalp '(1.0 -0.99 0.96 -0.92 0.84 -0.76 0.66)
      (interp #'cos 0 21.0 7))
  (is equalp '(1.0 0.31 -0.81 -0.83 0.28 0.99 0.34 -0.79 -0.84 0.25 0.99 0.37)
      (interp #'cos 0 15.0 12)))
(define-test test-log
  (is equalp '(0.0 0.88 1.34 1.65 1.89 2.08 2.25 2.39 2.51 2.62 2.71 2.8)
      (interp #'log 1 18.0 12))
  (is equalp '(0.0 1.09 1.6 1.94 2.19 2.39 2.56 2.7 2.83 2.94)
      (interp #'log 1 21.0 10)))

(define-test test-kata
  (is equalp '(0.69 1.0 1.24 1.43 1.59 1.73 1.85 1.96 2.06 2.15 2.23 2.3 2.37
               2.44 2.5)
      (interp #'log 2 13 15)))

      

(test :fp-approx)
