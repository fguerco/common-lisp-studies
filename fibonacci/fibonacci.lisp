(defpackage #:fibonacci
  (:use #:cl))

(in-package #:fibonacci)

;; recursive
(defun fib2 (n)
  (if (< n 2)
      n
      (+ (fib2 (1- n)) (fib2 (- n 2)))))

;; linear - recursion
(defun fib (n)
  (labels
      ((f (n a b)
         (if (= n 1)
             b
             (f (1- n) b (+ a b)))))
    (f n 0 1)))

;; linear - loop
(defun fibloop (n)
  (let ((fib 0) (a 0) (b 1))
    (dotimes (i n fib)
      (setq fib (+ a b))
      (setq b a)
      (setq a fib))))

