(defun plot (fn min max step)
  "Plot numbers based on the function passed"
  ;; line comment
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))


(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument 
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))     ; |


(defun counter (count)
  (list
   :inc (lambda () (incf count))
   :dec (lambda () (decf count))
   :get (lambda () count)))


(defun test-counter ()
  (let* ((counter (counter 0))
         (dec (getf counter :dec)))
    (assert (= -1 (funcall dec)))))


(defun no-negative-counter (counter)
  (let* ((dec (getf counter :dec))
         (get (getf counter :get)))
    (setf (getf counter :dec) (lambda () (if (> (funcall get) 0) (funcall dec) (funcall get))))
    counter))
    ;;(list
    ;; :inc inc
    ;; :dec (lambda () (if (> (funcall get) 0) (funcall dec) (funcall get)))
    ;; :get get)))

(defun test-no-negative-counter ()
  (let* ((counter (no-negative-counter (counter 2)))
         (dec (getf counter :dec)))
    (assert (= 1 (funcall dec)))
    (assert (= 0 (funcall dec)))
    (assert (= 0 (funcall dec)))))
  

(defun verbose-counter (counter)
  (let* ((inc (getf counter :inc))
         (dec (getf counter :dec))
         (get (getf counter :get)))
    (list
     :inc (lambda () (format t "incrementing ~a~%" (funcall get)) (funcall inc))
     :dec (lambda () (format t "decrementing ~a~%" (funcall get)) (funcall dec))
     :get (lambda () (format t "getting ~a~%" (funcall get)) (funcall get)))))



(format t "~%~%")
(do ((x 0 (1+ x))
     (y 10 (1- y)))
    ((= x y) t)
  (format t "x: ~a, y: ~a~%" x y))


(loop for x across "felipe" counting (find x "e"))

(defmacro run (bindings functions &body body)
  "Run a code block printing and evaluating the expressions"
  `(let* ,bindings
     (labels ,functions
       ,@(loop for form in body collect `(progn (format t "~%~%>>> ~a~%" ',form) (format t "~a~%" ,form))))))



(run () () "abc")

(run
    ()
    ()
    (dolist (x '(1 2 3 4))
      (print x)
      (print x)))

(run
    ((x (vector 'a 'b 'c)))
    ()
  x
  (length x)
  (elt x 1)
  (count 'b x))


(run
    ((x #(a b c))
     (v #((a 10) (b 20) (c 30) (d 40)))
     (l #(1 2 3 4 5))
     (r (reverse l)))
    ()
  x
  (length x)
  (elt x 1)
  (substitute #\_ #\e "felipe")
  (remove #\e "felipe")

  (find 'b v :key #'first)

  (setf (elt r 2) 99)

  l
  r)
    
(run
    ((ht (make-hash-table)))
    ((show-value (key hash-table)
       (multiple-value-bind (value present?) (gethash key hash-table)
         (format nil "Value ~a ~:[because key not found~;actually present.~]" value present?))))

  (setf (gethash 'foo ht) 10)
  (setf (gethash 'bar ht) nil)

  (show-value 'foo ht)
  (show-value 'bar ht)
  (show-value 'baz ht)

  (maphash (lambda (k v) (format t "~a => ~a~%" k v)) ht))



(run
    ((cell (cons 1 2)))
    ()
  (first cell)
  (rest cell)
  cell
  (setf (car cell) 10)
  cell
  (setf (cdr cell) 20)
  cell
  (setf (cdr cell) nil)
  cell)

(run
    ((list-1 (list 1 2))
     (list-2 (list 3 4))
     (list-3 (append list-1 list-2)))
    ()
  list-1
  list-2
  list-3

  (setf (first list-2) 0) ;; now list-2 and list-3 will be affected
  list-2
  list-3)




(run (set) ()

  (pushnew 1 set)
  (pushnew 2 set)
  (pushnew 2 set)

  (member 1 set)
  (find 2 set)

  (intersection '(1 2) '(2 3 4))
  set)


(run ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
    ()

  (assoc 'd alist)
  (rassoc 3 alist)

  (acons 'k 10 alist)
  (push (cons 'k 10) alist)

  alist

  (pairlis '(a b c) '(1 2 3)))



(run
    (plist)
    ()
  (setf (getf plist :a) 1)
  plist
  (setf (getf plist :a) 2)
  plist)
  

(defun bhaskara (a b c)
  (flet ((delta (a b c) (- (expt b 2) (* 4 a c)))
         (plus-or-minus (x y) (list (+ x y) (- x y)))
         (over-2a (x) (/ x (* 2 a))))

    (mapcar #'over-2a (plus-or-minus (- b) (sqrt (delta a b c))))))


(assert (equal (bhaskara 2 -16 -18) '(9.00 -1.00)))



(let ((ht (make-hash-table)))
  (setf (gethash 'a ht) 10)
  (setf (gethash 'b ht) 10)
  (print (maphash (lambda (k v) (cons k v)) ht)))

  
(defun add (x y)
  (if (= x 0)
      y
      (add (1- x) (1+ y))))
