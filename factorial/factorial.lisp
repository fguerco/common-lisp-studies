
(defun factorial (n)
  "Simple recursive factorial"
  (if (= n 1)
      1
      (* n (factorial (1- n)))))


(factorial 10)


(defun ! (n)
  "Funcional factorial"
  (reduce #'* (loop for i from 1 to n collect i)))

(! 10)

(defun factorial-loop (n)
  "Factorial using loop"
  (loop for i from 1 to n
        for f = i then (* f i)
        maximize f))

(factorial-loop 10)


