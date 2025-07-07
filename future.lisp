(ql:quickload "bordeaux-threads")

(defpackage :future
  (:use :cl))

(in-package :future)

(defstruct future
  thread function (status :created) value args)

(defmethod start ((f future))
  "Start execution"
  (with-slots (thread function status value args) f
    (unless (or thread (done-p f))
      (flet ((threadfn ()
               (setf value (apply function args)
                     status :done)))
        (setf thread (bt2:make-thread #'threadfn)
              status :running))))
  f)

(defmethod cancel ((f future))
  "Abort the execution"
  (with-slots (thread status value) f
    (when (and thread (bt2:thread-alive-p thread))
      (bt2:destroy-thread thread)
      (setf thread nil
            status :canceled
            value nil))))

(defmethod done-p ((f future))
  "Returns if the future is done"
  (eql :done (future-status f)))

(defmethod await ((f future))
  "Awaits for the future to complete and returns its value"
  (with-slots (value) f
    (if (done-p f)
        value
        (loop if (done-p f) return value))))

(defun future (value)
  "Make a future from a value"
  (make-future :status :done :value value))

(defmacro define-asyncfun (name args &body body)
  "Create an async function that returns a future"
  (let ((argnames (mapcan (lambda (x)
                            (cond ((listp x) (list (car x)))
                                  ((eq #\& (char (symbol-name x) 0)) nil)
                                  (t (list x))))
                          args)))
  `(defun ,name ,args
     (start (make-future :args (list ,@argnames)
                         :function (lambda ,args ,@body))))))



(defvar fut)

(define-asyncfun long-process (&optional (name "unnamed"))
  (format t "process name is ~a~%" name)
  (print "start")
  (sleep 5)
  (print "working")
  (sleep 5)
  (print "still working")
  (sleep 5)
  (print "done")
  :done)
