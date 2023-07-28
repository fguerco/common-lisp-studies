#|
Bell Numbers
https://rosettacode.org/wiki/Bell_numbers

Write a routine (function, generator, whatever) to generate the Bell number
sequence and call the routine to show here, on this page at least the first
15 and (if your language supports big Integers) 50th elements of the sequence.

If you do use the Bell triangle method to generate the numbers, also show the
first ten rows of the Bell triangle. 
|#

(defpackage #:bell-numbers
  (:use #:cl))

(in-package #:bell-numbers)

(defun bell-triangle (rows)
  (flet ((triangle-row (previous)
           (if previous
               (let ((row (last previous)))
                 (reduce (lambda (acc x)
                           (append acc (list (+ x (car (last acc))))))
                         previous :initial-value row))
               (list 1))))
    (nreverse (reduce (lambda (acc x)
              (declare (ignore x))
              (cons (triangle-row (car acc)) acc))
                      (make-array rows) :initial-value nil))))

(defun bell-numbers (n)
  (mapcar 'car (bell-triangle n)))


(defun print-numbers (n)
  (format t "~{~a~%~}" (bell-numbers n)))

(defun print-triangle (n)
  (format t "~{~{~a~^, ~}~%~}" (bell-triangle n)))

(print-numbers 15)
(print-numbers 50)
(print-triangle 10)
