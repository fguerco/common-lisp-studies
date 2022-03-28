
(defun bayes-formula (p-a p-b p-b-a)
  (/ (* p-b-a p-a) p-b))

(let* ((infection-prob (/ 1 1000))
       (false-positive (/ 1 100))
       (false-negative (/ 10 100))
       (true-positive (- 1 false-negative)))
  (bayes-formula infection-prob true-positive
  
