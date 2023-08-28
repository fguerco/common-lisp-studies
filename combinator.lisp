;; Y combinator as Lambda abstraction for lazy evaluation languages
(defun y (f)
  ((lambda (x)
     (funcall f (funcall x x)))
   (lambda (x)
     (funcall f (funcall x x)))))

;; this is to prevent infinite recursion
(defun y (f)
  ((lambda (x) (funcall x x))
   (lambda (x)
     (funcall f (lambda (&rest args)
                  (apply (funcall x x) args))))))
     

(y (lambda (f)
     (lambda (n)
       (if (zerop n) 1
           (* n (funcall f (1- n)))))))

;; long version without defun
((lambda (f)
   ((lambda (x) (funcall x x))
    (lambda (x)
      (funcall f (lambda (&rest args)
                   (apply (funcall x x) args))))))
 (lambda (f)
   (lambda (n)
     (cond ((zerop n) 1)
           (t (* n (funcall f (1- n))))))))

 
