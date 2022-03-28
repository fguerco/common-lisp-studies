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


(dotimes (b 99)
  (format t "~{~a~%~}" (bottle (- 99 b))))
