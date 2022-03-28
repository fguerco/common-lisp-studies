(defun avg (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

(defun square-root (x &optional (guess 1))
  (flet ((improve-guess (guess x) (avg guess (/ x guess)))
           (good-enough? (guess x tolerance) (< (abs (- (expt guess 2) x)) tolerance)))

    (if (good-enough? guess x .0001)
      (float guess)
      (square-root x (improve-guess guess x)))))

