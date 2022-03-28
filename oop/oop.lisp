(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen"))

(defmethod draw ((shape circle))
  ;; code
  )

(defmethod draw ((shape triangle))
  ;; code
  )


(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Customer name cannot be empty")
    :reader customer-name
    :writer (setf customer-name))
   (balance
    :initarg :balance
    :initform 0)
   (account-type :reader account-type)))


(defgeneric balance (account))
(defgeneric (setf balance) (value account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

(defmethod (setf balance) (value (account bank-account))
  (setf (slot-value account 'balance) value))


;; constructor
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (balance account)))
    (setf (slot-value account 'account-type)
          (cond
            ((> balance 10000) :gold)
            ((> balance 5000) :silver)
            (t :bronze)))))

(defclass checking-account (bank-account) ())
(defclass savings-account (bank-account) ())
(defclass overdraft-account (bank-account) ())

(let ((acc (make-instance 'bank-account :customer-name "Jumeci" :balance 150)))
  (withdraw acc 80)
  (balance acc))

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))


(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))
    (call-next-method)))


;; this eql will be evaluated only once - when the defmethod is evaluated
(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
    (call-next-method)))


(defgeneric op (x)
  (:documentation "Do operation on x"))

(defmethod op (x)
  (print "call op")
  x)

(defmethod op ((x number))
  (print "call op number")
  (+ x 1)
  (call-next-method))

(defmethod op ((x integer))
  (print "call op integer")
  (call-next-method)
  (+ x 2)
  )

(defmethod op ((x string))
  (print "call op string")
  (format nil "x = ~a" x)
  (call-next-method))

(defmethod op ((l list))
  (print "call op list")
  (call-next-method)
  (format nil "~{~a, ~}" l))


(defgeneric gen-list (x)
  (:documentation "Example of a generic function that lists its returns")
  (:method-combination list))

(defmethod gen-list list (x)
  (print "base method")
  x)

(defmethod gen-list list ((x number))
  (print "> number")
  (* x 10))

(defmethod gen-list list ((x float))
  (print "> float")
  (* x 100))

(defmethod gen-list list ((x string))
  (print "> string")
  (format nil "x: ~a" x))



(defgeneric call-before (x)
  (:documentation "Return the value of X depending on data type"))

(defmethod call-before (x)
  (print "- base")
  x)

(defmethod call-before :before ((x number))
  (print "- number")
  (* x 10))

(defmethod call-before :before ((x integer))
  (print "- integer")
  (* x 100))

(call-before 10)

;; "- integer" 
;; "- number" 
;; "- base" 
;; 10


(defgeneric call-after (x)
  (:documentation "Return the value of X depending on data type"))

(defmethod call-after (x)
  (print "- base")
  x)

(defmethod call-after :after ((x number))
  (print "- number")
  (* x 10))

(defmethod call-after :after ((x integer))
  (print "- integer")
  (* x 100))


(call-after 20)

;; "- base" 
;; "- number" 
;; "- integer" 
;; 20

(load "/home/felipe/dev/lisp/study/uuid.lisp")

(defclass customer ()
  ((id :initform (uuid-v4)
       :reader id)
   (name :initarg :name
         :initform (error "Name cannot be blank")
         :accessor name)))

(let ((customer (make-instance 'customer :name "Jumeca")))
  (with-accessors ((id id)
                   (name name)) customer
    (format t "~%id: ~a, name: ~a~%" id name)))


(defclass product ()
  ((id :initform (uuid-v4)
       :reader id)
   (name :initarg :name
         :accessor name)
   (api-version :accessor api-version
                :initform "v1"
                :allocation :class)))

(let ((pr1 (make-instance 'product :name "Product ABC"))
      (pr2 (make-instance 'product :name "Product DEF")))
  (with-accessors ((id1 id) (name1 name) (av1 api-version)) pr1
    (with-accessors ((id2 id) (name2 name) (av2 api-version)) pr2
      (format t "~%id: ~a, name: ~a, api version: ~a~%" id1 name1 av1)
      (format t "~%id: ~a, name: ~a, api version: ~a~%" id2 name2 av2)
      (setf av2 "v2")
      (format t "v1: ~a, v2: ~a~%" av1 av2))))
   
