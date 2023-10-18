(defpackage #:pmatch
  (:use #:cl)
  (:export #:_
           #:match
           #:match-if
           #:match-bind
           #:match-case))

(in-package #:pmatch)

;;;; Very simple and basic pattern matching

(defun flatten (x)
  (if (listp x)
      (mapcan #'flatten x)
      (list x)))

(defun variablep (x)
  (and (symbolp x)
       (not (eql '_ x))))

(defun extract-variables (pattern)
  (remove-if-not #'variablep (flatten pattern)))
      
(defmethod match ((a symbol) b)
  (if (and (symbolp b) (find-symbol (string b)))
      (values nil nil)
      (values (list (cons a b)) t)))

(defmethod match ((a (eql '_)) b)
  (values nil t))

(defmethod match ((a string) (b string))
  (values nil (string= a b)))

(defmethod match (a b)
  (values nil (eql a b)))

(defmethod match ((a list) (b list))
  (loop for p in a
    and l in b
    for (x found) = (multiple-value-list (match p l))
    unless found return (values nil nil)
    nconc x into result
    finally (return (values result t))))
    
(defmacro match-if (pattern form then &optional else)
  (let ((vars (extract-variables pattern))
        (result (gensym))
        (found (gensym)))
     `(multiple-value-bind (,result ,found)
         (match ',pattern ,form)
       (if ,found
           (destructuring-bind ,vars
               (mapcar #'cdr ,result)
             ,then)
           ,else))))

(defmacro match-bind (pattern form &body body)
  `(match-if ,pattern ,form
             (progn ,@body)))

(defmacro %match-case (form cases)
  (when cases
    (destructuring-bind ((pat . body) . rest)
        cases
      `(match-if ,pat ,form
                 (progn ,@body)
                 (%match-case ,form ,rest)))))

(defmacro match-case (form &body cases)
  (let ((fvar (gensym)))
    `(let ((,fvar ,form))
       (%match-case ,fvar ,cases))))
      


(let ((val '(1 "oi" (3 4 (999 :done)))))
  (match-bind (x j (y _ (_ z))) val
    (list j x y z)))

(let ((val '(1 2 99)))
  (match-case val
    ((x 20)
     (format t "x: ~a~%" x))
    ((_ _ 2)
     (format t "just 99~%"))
    ((x 2 y)
     (format t "x: ~a, y: ~a~%" x y))))

(let ((val '(10 20 (30 (40) 50))))
  (match-if (_ 20 (x _ 50)) val
            (format nil "deu bom: ~a!!" x)
            "hmmm :("))
