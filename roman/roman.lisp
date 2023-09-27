(ql:quickload "parachute" :silent t)

(defpackage :roman
  (:use :cl :parachute))

(in-package :roman)

(defvar *roman-table* '((1000 . M) (900 . CM) (500 . D) (400 . CD)
                        (100 . C) (90 . XC) (50 . L) (40 . XL)
                        (10 . X) (9 . IX) (5 . V) (4 . IV)
                        (1 . I)))


(defun encode (number)
  "Encode decimal to roman"
  ;; simplest possible would be format with ~@R
  (loop for n = number then (- n dec)
        while (plusp n)
        for (dec . roman) = (assoc-if (lambda (x) (<= x n)) *roman-table*)
        collect roman into result
        finally (return (format nil "~{~a~}" result))))
        
        
(define-test encode ()
  (is equal "MMMCMXCIX" (encode 3999))
  (is equal "MMII" (encode 2002))
  (is equal "MCMLXXXIII" (encode 1983)))


(defun decode (roman)
  "Decode roman to decimal"
  (loop for prev = 0 then n
        for c across (reverse roman)
        for (n . rom) = (rassoc c *roman-table* :test #'string=)
        sum (if (> prev n) (- n) n)))

(define-test decode ()
  (is = 3999 (decode "MMMCMXCIX"))
  (is = 2002 (decode "MMII"))
  (is = 1983 (decode "MCMLXXXIII")))
