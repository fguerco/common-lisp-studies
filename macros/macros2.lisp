(defpackage :felipe
  (:use :cl))

(in-package :felipe)


(defun expr (x)
  (progn (format t "expr ~a~%" x) x))



(defmacro andf (&rest forms)
  (labels ((add-if (forms)
             (if (cdr forms)
                 `(if ,(car forms) ,(add-if (cdr forms)))
                 (car forms))))
    (add-if forms)))


(defmacro andf (&rest forms)
  (if (cdr forms)
      `(if ,(car forms) (andf ,@(cdr forms)))
      (car forms)))



(defmacro orf (&rest forms)
  (if (cdr forms)
      (let ((var (gensym)))
        `(let ((,var ,(car forms)))
           (if ,var
               ,var
               (orf ,@(cdr forms)))))
      (car forms)))

(defmacro condf (&rest clauses)
  `(if ,(caar clauses)
       ,(cadar clauses)
       ,(and (cdr clauses) `(condf ,@(cdr clauses)))))


(condf
 ((expr nil) (expr 2))
 ((expr 2) (expr 4))
 ((expr nil) (expr 6)))


(orf (expr nil) (expr nil) (expr nil) (expr nil) (expr 5) (expr 6))


(let ((forms '(1 2 3 4 5)))
  (reduce #'(lambda (acc it) (list 'if acc it)) forms :from-end t))

