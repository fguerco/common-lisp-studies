(defun term-variable (term)
  (let ((v
          (cond
            ((and (atom term) (not (numberp term))) term)
            ((not (atom term)) (remove-if (lambda (x) (or (numberp x) (find x '(+ - * /)))) term))
            (t nil))))
    (if (or (atom v) (cdr v)) v
        (car v))))

(defun term-coefficient (term)
  (let ((c (cond
             ((and (atom term) (numberp term)) term)
             ((and (atom term) (not (numberp term))) 1)
             ((not (atom term)) (or (remove-if (lambda (x)
                                                 (or (not (numberp x)) (find x '(+ - * /))))
                                               term)
                                    1))
             (t nil))))
    (if (or (atom c) (cdr c)) c
        (car c))))

(defun like-terms? (a b)
  (equal (term-variable a) (term-variable b)))


(defun sum-like-terms (a b)
  (list
   (+ (or (term-coefficient a) 0)
      (or (term-coefficient b) 0))
   (term-variable a)))

(defun find-like-term (a b)
  (find a b :test #'like-terms?))

(defun sum-polynomials (a b)
  (cons '+ (mapcar (lambda (it)
            (sum-like-terms it (find-like-term it b)))
          a)))
          

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
    (= 1 (term-coefficient 'x))))


(deftest test-like-terms? ()
  (check
    (like-terms? '(* x x) '(* 2 x x))
    (like-terms? 'x '( * -1 x))
    (like-terms? '(* -10 x x x) '(x x x))
    (not (like-terms? '(* 2 x x) 'x))
    (not (like-terms? '(* 2 x) '(* 2 y)))))

(deftest test-sum-like-terms ()
  (check
    (equal 10 (sum-like-terms 7 3))
    (equal '(* 2 x) (sum-like-terms 'x 'x))
    (equal '(* 0 x) (sum-like-terms '(* 1 x) '(* -1 x)))))

(deftest test-find-like-term ()
  (check
    (equal '(* 10 x) (find-like-term 'x '(+ (* 2 x x) (* 10 x) -3)))))

(deftest test-polynomials ()
  (test-term-variable)
  (test-term-coefficient)
  (test-like-terms?)
  (test-sum-like-terms)
  (test-find-like-term))

