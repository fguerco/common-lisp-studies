(defpackage #:currying
  (:use #:cl))

(in-package #:currying)

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


(defmacro defcurry (name args &body forms)
  (unless (>= (length args) 2)
    (error "should have two or more arguments"))
  `(defun ,name (,(car args))
     ,(labels ((add-fn (args)
                 (if args
                     `(lambda (,(car args))
                        ,(add-fn (cdr args)))
                     `(progn
                        ,@forms))))
        (add-fn (cdr args)))))

