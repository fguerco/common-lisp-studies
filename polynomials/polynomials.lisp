(defstruct (term (:constructor make-term (const &optional var var-exp)))
  const var var-exp)

(defun like-terms? (a b)
  (equal (list (term-var a) (term-var-exp a))
         (list (term-var b) (term-var-exp b))))


(defun sum-like-terms (a b)
  (make-term (+ (term-const a) (term-const b))
             (term-var a)
             (term-var-exp a)))

(defun find-like-term (a b)
  (find a b :test #'like-terms?))

(defun sum-polynomials (a b)
  (loop for term-a in a
        for term-b = (find-like-term term-a b)
        if (like-terms? term-a term-b)
          collect (sum-like-terms term-a term-b)))
          

(sum-polynomials (list (make-term 2 'x 2) (make-term -10 'x 1) (make-term 5))
                 (list (make-term -1 'x 2) (make-term 4 'x  1) (make-term 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Tests                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-like-terms? ()
  (check
    (like-terms? (make-term 1 'x 2) (make-term 2 'x 2))
    (like-terms? (make-term 1 'x) (make-term -1 'x))
    (like-terms? (make-term -10 'x 3) (make-term 1 'x 3))
    (not (like-terms? (make-term 2 'x 2) (make-term 1 'x 1)))
    (not (like-terms? (make-term 2 'x 1) (make-term 2 'y 1)))))

(deftest test-sum-like-terms ()
  (check
    (equalp (make-term 10) (sum-like-terms (make-term 7) (make-term 3)))
    (equalp (make-term 2 'x 1) (sum-like-terms (make-term 1 'x 1) (make-term 1 'x 1)))
    (equalp (make-term 0 'x 1) (sum-like-terms (make-term 1 'x 1) (make-term -1 'x 1)))
    (equalp (make-term 2 'x 2) (sum-like-terms (make-term 1 'x 2) (make-term 1 'x 2)))))

(deftest test-find-like-term ()
  (check
    (equalp (make-term 10 'x 1) (find-like-term (make-term 1 'x 1)
                                               (list (make-term 2 'x 2)
                                                     (make-term 10 'x 1)
                                                     (make-term -3))))))

(deftest test-sum-polynomials ()
  (check
    (equalp (list (make-term 1 'x 2)
                  (make-term -4 'x 1)
                  (make-term 13))
            (sum-polynomials (list (make-term 2 'x 2)
                                   (make-term -6 'x 1)
                                   (make-term 5))
                             (list (make-term -1 'x 2)
                                   (make-term 2 'x 1)
                                   (make-term 8))))))

(deftest test-polynomials ()
  (test-like-terms?)
  (test-sum-like-terms)
  (test-find-like-term)
  (test-sum-polynomials))

