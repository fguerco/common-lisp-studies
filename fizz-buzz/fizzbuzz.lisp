#!/usr/bin/sbcl --script

(defun special-case (num text)
  (lambda (n) (if (zerop (rem n num)) text)))


(defun special-cases (num &rest cases)
  (or
   (loop for case in cases as value = (funcall case num) when value collect value)
   (list num)))


(defun fizz-buzz (num)
  (format nil "~{~a~}"
          (special-cases num (special-case 3 "Fizz") (special-case 5 "Buzz"))))

(defun fizz-buzz-seq (max)
  (loop for n from 1 to max collect (fizz-buzz n)))

(format t "~&~{~a~^, ~}~%"
         (fizz-buzz-seq 100))
