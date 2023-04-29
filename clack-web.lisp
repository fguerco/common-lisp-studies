(ql:quickload :clack)
(ql:quickload :lack)


(defun handler (env)
  (declare (ignore env))
  '(200 nil ("Server Ok")))

(defun api-v1-handler (env)
  (declare (ignore env))
 '(200 nil ("api/v1 ok")))

(trace handler)

(defparameter *api-v1*
  (lack:builder
   (lambda (env) (funcall 'api-v1-handler env))))

(defparameter *app*
  (lack:builder
   :accesslog
   (:mount "/api/v1" #'api-v1-handler)
   (lambda (env) (funcall 'handler env))))

(defparameter *clack-server* (clack:clackup *app*))


(clack:stop *clack-server*)
