(defun operator? (term)
  (find term '(+ - * /)))

(defun filter-variable (term)
  (remove-if (lambda (x) (or (numberp x) (operator? x))) term))

(defun filter-constant (term)
  (remove-if (lambda (x) (or (not (numberp x)) (operator? x))) term))


(defun term-variable (term)
  (let ((v
          (cond
            ((and (atom term) (not (numberp term)) (not (operator? term))) term)
            ((not (atom term)) (filter-variable term))
            (t nil))))
    (if (or (atom v) (cdr v)) v
        (car v))))

(defun term-coefficient (term)
  (let ((c (cond
             ((and (atom term) (numberp term)) term)
             ((and (atom term) (not (numberp term)) (not (operator? term))) 1)
             ((not (atom term)) (or (filter-constant term) 1))
             (t nil))))
    (if (or (atom c) (cdr c)) c
        (car c))))

(defun is-term? (term)
  (or (term-variable term) (term-coefficient term)))

(defun like-terms? (a b)
  (and a b (equal (term-variable a) (term-variable b))))


(defun sum-like-terms (a b)
  (let ((term-variable (term-variable a))
        (coefficient-a (or (term-coefficient a) 0))
        (coefficient-b (or (term-coefficient b) 0)))
    (cond
      ((null term-variable) (+ coefficient-a coefficient-b))
      ((listp term-variable) (append (list '* (+ coefficient-a coefficient-b)) term-variable))
      (t  (list '* (+ coefficient-a coefficient-b) term-variable)))))


(defun find-like-term (a b)
  (find a b :test #'like-terms?))

(defun remove-operator (polynomial)
  (cdr polynomial))


(defun sum-polynomials (a b)
  (loop for term-a in (remove-operator a)
        for term-b = (find-like-term term-a (remove-operator b))
        if (and (is-term? term-a) (like-terms? term-a term-b))
          collect (sum-like-terms term-a term-b) into sum
        finally (return (if sum (cons '+ sum) nil))))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Tests                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-term-variable ()
  (check
    (null (term-variable 2))
    (eq 'x (term-variable 'x))
    (eq 'x (term-variable '(* 2 x)))
    (equal '(x x) (term-variable '(* 10 x x)))))


(deftest test-term-coefficient ()
  (check
    (= 2 (term-coefficient 2))
    (= 1 (term-coefficient '(* x x)))
    (= 2 (term-coefficient '(* 2 x)))
    (= 1 (term-coefficient 'x))
    (not (term-coefficient '+))))


(deftest test-like-terms? ()
  (check
    (like-terms? '(* x x) '(* 2 x x))
    (like-terms? 'x '( * -1 x))
    (like-terms? '(* -10 x x x) '(x x x))
    (not (like-terms? '(* 2 x x) 'x))
    (not (like-terms? '(* 2 x) '(* 2 y)))))

(deftest test-sum-like-terms ()
  (check
    (= 10 (sum-like-terms 7 3))
    (equal '(* 2 x) (sum-like-terms 'x 'x))
    (equal '(* 0 x) (sum-like-terms '(* 1 x) '(* -1 x)))
    (equal '(* 2 x x) (sum-like-terms '(* x x) '(* x x)))))

(deftest test-find-like-term ()
  (check
    (equal '(* 10 x) (find-like-term 'x '(+ (* 2 x x) (* 10 x) -3)))))

(deftest test-sum-polynomials ()
  (check
    (equal '(+ (* 1 x x) (* -4 x) 13)
           (sum-polynomials '(+ (* 2 x x) (* -6 x) 5) '(+ (* -1 x x) (* 2 x) 8)))))

(deftest test-polynomials ()
  (test-term-variable)
  (test-term-coefficient)
  (test-like-terms?)
  (test-sum-like-terms)
  (test-find-like-term)
  (test-sum-polynomials))

