(defun say-bottle (num) "return the number of bottles - singular, plural or special case zero"
  (cond ((= num 0) "No more bottles")
	((= num 1) "1 bottle")
	(t (format nil "~a bottles" num))))

(defun bottle (num) "return the verse for a number of bottles"
  (list
   (format nil "~a of beer on the wall" (say-bottle num))
   (format nil "~a of beer" (say-bottle num))
   "Take one down, pass it around"
   (format nil "~a of beer" (say-bottle (1- num)))))


(defun bottles (&key
                  (current 1)
		  (stop 99)
		  acc) "return many verses, from current"
  
  (if (> current stop)
      acc
      (bottles :current (1+ current)
	       :stop stop
	       :acc (push (bottle current) acc))))

(defun print-data (list)
  (format t "~%~{~%~{~a~%~}~}" list))


(print-data (bottles))

;; (format t "~{~a~}" '(1 2 3))



;; `(1 ,@`(2) ,@`(3) 3)

(defun loop-bottles ()
  (dotimes (b 99)
    (format t "ola ~d~%" (- 99 b))))


(loop-bottles)



(defmacro run-down (key value data &body body)
  (let ((local-data (gensym)))
    `(let ((,local-data ,data))
       (do while ,local-data
         (let ((,key (pop ,local-data)) (,value (pop ,local-data)))
           ,@body)))))
;;  `(print data))


(run-down k v '(a 1 b 2 c 3)
  (print k)
  (print v))


  let #:G455 params
    let i #:G455

(run-down '(a 1 b 2 c 3))
