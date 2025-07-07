;; Y combinator as Lambda abstraction for lazy evaluation languages
(defun y (f)
  (funcall
   (lambda (x)
     (funcall f (funcall x x)))
   (lambda (x)
     (funcall f (funcall x x)))))

;; this is to prevent infinite recursion
(defun y* (f)
  (funcall
   (lambda (x) (funcall x x))
   (lambda (x)
     (funcall f (lambda (&rest args)
                  (apply (funcall x x) args))))))

;; the Z combinator also for eager languages
(defun z (f)
  (funcall
   (lambda (x)
     (funcall f (lambda (v) (funcall (funcall x x) v))))
   (lambda (x)
     (funcall f (lambda (v) (funcall (funcall x x) v))))))


;;; the factorial function to run recursively
(defparameter fac
  (lambda (f)
    (lambda (n)
      (if (zerop n) 1
          (* n (funcall f (1- n)))))))

;; long version without defun using y*
((lambda (f)
   ((lambda (x) (funcall x x))
    (lambda (x)
      (funcall f (lambda (&rest args)
                   (apply (funcall x x) args))))))
 (lambda (f)
   (lambda (n)
     (cond ((zerop n) 1)
           (t (* n (funcall f (1- n))))))))

 
