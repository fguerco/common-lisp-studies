(defpackage :dev.felipe.lisp-studies.fibonacci (:use :common-lisp))
(in-package :dev.felipe.lisp-studies.fibonacci)

(defun fibonacci-recursive (n)
  "Returns the Fibonacci number using recursive strategy"
  (if
   (< n 2)
   n
   (+ (fib (1- n)) (fib (- n 2)))))


(defun fibondacci-loop (n)
  "Returns a Fibonacci sequence of n numbers"
  (loop
     with current = 1
     with previous = 0
     for sum = (+ previous current)
     for k from 1 to n
     do (setf previous current)
     do (setf current sum)
     collect current))


(fibonacci-recursive 45)
