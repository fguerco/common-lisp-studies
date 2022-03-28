

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))


(defmacro with-gensyms (( &rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


(defmacro doprimes ((var start end) &body body)
  (with-gensyms (var-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,var-name ,end))
         ((> ,var ,var-name))
       ,@body)))


(doprimes (p 1 19)
  (print p)
  (format t "~a " p))


(defmacro run-down (key value data &body body)
  (let ((local-data (gensym)))
    `(let ((,local-data ,data))
       (loop for ,key = (pop ,local-data)
             and ,value = (pop ,local-data)
             do (progn ,@body)
             while ,local-data))))


(run-down k v '(a 1 b 2 c 3)
  (format t "k=~a, v=~a~%" k v))



(defmacro run (bindings functions &body body)
  "Run a code block printing and evaluating the expressions"
  `(let* ,bindings
     (labels ,functions
       ,@(loop for form in body collect `(progn (format t "~%~%>>> ~s~%" ',form) (format t "~a~%" ,form))))))


(run () ()
  (+ 1 2)
  (format nil "x: ~a" 10))
