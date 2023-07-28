(defpackage #:clj-threads
  (:use #:cl))

(in-package #:clj-threads)

(defmacro -> (value &body forms)
  (let ((form (apply #'list (caar forms) value (cdar forms))))
    (cond
      ((cdr forms) `(-> ,form ,@(cdr forms)))
      (t form))))

(defmacro ->> (value &body forms)
  (let ((form `(,@(car forms) ,value)))
    (cond
      ((cdr forms) `(->> ,form ,@(cdr forms)))
      (t form))))

(defmacro as-> ((var value) &body forms)
  (cond
    ((cdr forms) `(as-> (,var ,(subst value var (car forms))) ,@(cdr forms)))
    (t (car (subst value var forms)))))



;; Usage

(-> "ab cd ef gh"
  (string-capitalize :start 5)
  (reverse))

; => "hG fE dc ba"


(->> '(1 2 3 4 5)
  (remove-if-not #'oddp)
  (mapcar #'/))

; => (1 1/3 1/5)

(as-> ($ "12345")
  (map 'list #'string $)
  (mapcar #'parse-integer $)
  (sort $ #'>)
  (remove-if-not #'oddp $)
  (mapcar #'/ $))

; => (1/5 1/3 1)


(as-> (l (list 1 2 3 4))
  (remove-if-not #'oddp l)
  (mapcar (lambda (x) (* x 2)) l)
  (sort l #'>))

; => (6 2)

(as-> (n 0)
  (1+ n)
  (1+ n))

; => 2
