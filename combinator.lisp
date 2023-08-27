((lambda (zero? mult dec)
  ((lambda (x)
     ((lambda (rec x)
        (funcall rec rec x))
      (lambda (self x)
        (if (funcall zero? x)
            1
            (funcall mult x (funcall self self (funcall dec x)))))
      x))
     5))
   (lambda (x) (= x 0))
   (lambda (x y) (* x y))
   (lambda (x) (- x 1)))
           
