(ql:quickload "parachute" :silent t)

(defpackage :roman
  (:use :cl :parachute))

(in-package :roman)

(defvar *roman-table* '((1000 . "M") (900 . "CM")
                        (500 . "D") (400 . "CD")
                        (100 . "C") (90 . "XC")
                        (50 . "L") (40 . "XL")
                        (10 . "X") (9 . "IX")
                        (5 . "V") (4 . "IV")
                        (1 . "I")))

(defun decimal->roman (number)
  "Returns roman numeral for NUMBER"
  (when (plusp number)
    (destructuring-bind (dec . roman)
        (assoc number *roman-table* :test #'>=)
      (concatenate 'string roman (decimal->roman (- number dec))))))

(defun dec->rom (n)
  "Less readable but faster. Based on SBCL's implementation found in format"
  (with-output-to-string (st)
    (loop
      for roman in '(#\M #\D #\C #\L #\X #\V #\I)
      and dec in '(1000 500 100 50 10 5 1)
      and rprev in '(#\C #\C #\X #\X #\I #\I nil)
      and nprev in '(100 100 10 10 1 1 0)
      until (zerop n)
      do (loop
           while (>= n (- dec nprev))
           if (>= n dec)
             do (decf n dec)
                (write-char roman st)
           else
             do (decf n (- dec nprev))
                (write-char rprev st)
                (write-char roman st)))))


(defmacro gen-encode-test-data ()
  `(progn
     ,@(loop
         repeat 50
         for n = (1+ (random 3999))
         collect `(is equal ,(format nil "~@r" n) (decimal->roman ,n))
         collect `(is equal ,(format nil "~@r" n) (dec->rom ,n)))))

(define-test encode ()
  (gen-encode-test-data))

(defun roman->decimal (roman)
  "Decode roman to decimal"
  (loop for prev = 0 then n
        for c across (reverse roman)
        for (n . rom) = (rassoc c *roman-table* :test #'string=)
        sum (if (> prev n) (- n) n)))

(defmacro gen-decode-test-data ()
  `(progn
     ,@(loop
         repeat 50
         for n = (1+ (random 3999))
         collect `(is = ,n (roman->decimal ,(format nil "~@r" n))))))

(define-test decode ()
  (gen-decode-test-data))

    
