
(defun greatest-common-denominator (x y)
  "Finds the greatest common divisor between the two numbers using euclidean method"
  (if (= y 0)
      x
      (greatest-common-denominator y (mod x y))))

(defun make-rat (n d)
  "Create a rational number with numerator n and denominator d"
  (let ((gcd (greatest-common-denominator n d)))
    (cons (/ n gcd) (/ d gcd))))

(defun numer (rat)
  "Obtain the numerator"
  (first rat))

(defun denom (rat)
  "Obtain the denominator"
  (rest rat))

(defun +-rat (x y)
  "Returns the sum two rational numbers"
  (let
      ((numer-x (numer x))
       (denom-x (denom x))
       (numer-y (numer y))
       (denom-y (denom y)))
    (make-rat (+ (* numer-x denom-y)
                 (* numer-y denom-x))
              (* denom-x denom-y))))

(defun *-rat (x y)
  "Returns the product of two rational numbers"
  (let
      ((numer-x (numer x))
       (denom-x (denom x))
       (numer-y (numer y))
       (denom-y (denom y)))
    (make-rat (* numer-x numer-y)
              (* denom-x denom-y))))

(+-rat (make-rat 1 2) (make-rat 1 4))
(*-rat (make-rat 1 2) (make-rat 1 4))
