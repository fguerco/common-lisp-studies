(defun memoize (fn &key (test #'equal))
  (let ((cache (make-hash-table :test test)))
    (lambda (&rest args)
      (multiple-value-bind (v f) (gethash args cache)
        (if f
            v
            (setf (gethash args cache)
                  (apply fn args))))))) 


(defmacro define-memoized (name args &body body)
  (let ((f (gensym)) (m (gensym)))
    `(flet ((,f ,args ,@body))
       (let ((,m (memoize #',f)))
         (defun ,name ,args
            (funcall ,m ,@(args-from-lambda-list args)))))))


(defun memofib (n)
  (if (< n 2)
      1
      (+ (memofib (1- n))
         (memofib (- n 2)))))


(define-memoized memofib (n)
   (if (< n 2)
       1
       (+ (memofib (1- n))
          (memofib (- n 2)))))


(define-memoized myfn (n &key (invert t))
  (sleep 3)
  (if invert (/ n) n))
