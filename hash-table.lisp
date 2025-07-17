;; a simple (and naive) hash table implementation

(defun -hash (value slots)
  (rem (sxhash value) slots))


(defclass -hashtable ()
  ((max-size :reader max-size :initarg :max-size)
   (data :reader data)))


(defmethod initialize-instance :after ((ht -hashtable)
                                       &key max-size)
  (setf (slot-value ht 'data)
        (make-array max-size :initial-element nil)))


(defmethod put ((ht -hashtable) key value)
  (with-slots (max-size data) ht
    (let ((index (-hash key max-size)))
      (symbol-macrolet ((item (elt data index)))
        (let ((pos (position key item :key #'car :test #'equal)))
          (if pos
              (setf (elt item pos) (cons key value))
              (push (cons key value) item))
          value)))))


(defmethod lookup ((ht -hashtable) key)
  (with-slots (data max-size) ht
    (let* ((index (-hash key max-size))
           (slot (elt data index)))
      (if slot
          (let ((item (assoc key slot :test #'equal)))
            (values (cdr item) (consp item)))
          (values nil nil)))))


(defmethod take ((ht -hashtable) key)
  (with-slots (data max-size) ht
    (let ((index (-hash key max-size)))
      (symbol-macrolet ((item (elt data index)))
        (let ((elem (find key item :key #'car :test #'equal)))
          (print elem)
          (when elem
            (setf item (remove elem item))
            (cdr elem)))))))
      

(defmethod get-or-put ((ht -hashtable) key fn)
  (multiple-value-bind (value found) (lookup ht key)
    (if found
        value
        (put ht key (funcall fn key)))))


;; Test
(let ((ht (make-instance '-hashtable :max-size 20)))
  (dotimes (x 30)
    (put ht x (format nil "element number ~a" x)))
  
  (format t "Data: ~a~%" (data ht))
  (put ht 1 "Helo")
  (put ht 2 "World")
  (put ht 99 "To remove")
  (put ht 450 nil)
  (format t "Data: ~a~%" (data ht))
  (format t "removed: ~a~%" (take ht 200))
  (format t "28 = ~a~%" (lookup ht 28))
  (format t "16 = ~a~%" (lookup ht 16))
  (get-or-put ht 999 (lambda (x) (format nil "value of ~a" x ))))
