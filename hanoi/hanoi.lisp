(defpackage :hanoi
  (:use :cl))

(in-package :hanoi)

;; tower of hanoi
(defun move (n from to spare)
  (when (plusp n)
    (move (1- n) from spare to)
    (format t "moving ~d from ~d to ~d with ~d as spare~%" n from to spare)
    (move (1- n) spare to from)))


(move 3 'a 'c 'b)
