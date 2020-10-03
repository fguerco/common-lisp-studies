(defun auto-curry (fn nargs)
  (when (< nargs 2)
    (error "nargs should be two or more"))
  (labels ((curry (args)
             (if (= (length args) nargs)
                 (apply fn (nreverse args))
                 (lambda (arg)
                   (curry (cons arg args))))))
  (curry nil)))



(defparameter *incr* (funcall (auto-curry #'+ 2) 1))
(funcall *incr* 5)

(defparameter *tri-divide* (auto-curry #'/ 3))
(funcall (funcall (funcall *tri-divide* 1) 3) 5)
