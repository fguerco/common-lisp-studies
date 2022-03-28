(defun map2 (fn list)
  (when list
      (cons (funcall fn (car list))
            (map2 fn (cdr list)))))

(defun foreach (fn list)
  (when list
    (funcall fn (car list))
    (foreach fn (cdr list))))


(defun reduce2 (fn list &optional (initial-value nil initial-value-supplied?))
  (if initial-value-supplied?
      (let ((value (funcall fn initial-value (car list))))
        (if (cdr list)
            (reduce2 fn (cdr list) value)
            value))
      (reduce2 fn (cdr list) (car list))))


(reduce2 #'+ '(1 2 3 4 5 6 7 8 9 10))



(map2 (lambda (x) (* x 2)) '(1 2 3 4 5))
