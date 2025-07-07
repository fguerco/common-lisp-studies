
(defun pascal-row (previous)
  "Pascal triangle row with reduce"
  (let (row)
    (reduce (lambda (prev it)
              (push (+ prev it) row)
              it)
            previous :initial-value 0)
    (reverse (cons 1 row))))


(defun pascal-row (previous)
  "Pascal triangle row with dolist"
  (let (result
        (acc 0))
    (dolist (x previous (reverse (cons 1 result)))
      (push (+ x acc) result)
      (setf acc x))))
    

(defun pascal-row (previous)
  "Pascal triangle row with loop"
  (loop with len = (length previous)
        for i from 1
        for x in previous
        and prev = 0 then x
        collect (+ prev x)
        when (= i len) collect 1))

(defun pascal (y x)
  "Pascal number by running down the pyramid"
  (loop for i from 1 upto y
        for row = (list 1) then (pascal-row row)
        finally (return (nth (1- x) row))))

(defun pascal-pyramid (rows)  
  (loop for i from 1 upto rows
        for row = (list 1) then (pascal-row row)
        do (format t "~{~a ~}~%" row)))


(defun fact (x)
  (let ((f 1))
    (dotimes (i x f)
      (setf f (* f (1+ i))))))

(defun pascal (y x)
  "Pascal number by binomial coefficients"
  (decf y)
  (decf x)
  (/ (fact y)
     (* (fact x)
        (fact (- y x)))))
