;; exponential
(defun fib2 (x)
  (if (< x 2)
      x
      (+ (fib2 (1- x)) (fib2 (- x 2)))))

;; linear
(defun fib (n &optional (x 0) (y 1))
  (if (= n 1)
      y
      (fib (1- n) y (+ x y))))

(loop for i from 1 to 10
      collect (fib i))

