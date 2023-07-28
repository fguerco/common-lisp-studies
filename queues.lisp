;; closure approach

(defpackage #:queue/closure
  (:use #:cl))
(in-package #:queue/closure)

(defun make-queue ()
  (let ((queue nil)
        (tail nil))
    (lambda (op &rest args)
      (flet ((qpush (x)
               (let ((x (list x)))
                 (if queue
                     (rplacd tail x)
                     (setf queue x))
                 (setf tail x)))
             (qpop ()
               (when queue
                 (prog1
                     (car queue)
                   (setf queue (cdr queue))))))
        (case op
          (push (apply #'qpush args))
          (pop (qpop))
          (show (values queue tail)))))))


;; functions approach with cons cell

(defpackage #:queue/functions
  (:use #:cl))
(in-package #:queue/functions)

(defun make-queue ()
  (cons nil nil))

(defun queue-push (queue item)
  (let ((item (list item)))
    (if (car queue)
        (rplacd (cdr queue) item)
        (rplaca queue item))
    (rplacd queue item)))

(defun queue-pop (queue)
  (when (car queue)
    (prog1
        (caar queue)
      (rplaca queue (cdar queue)))))



;; struct approach

(defpackage #:queue/struct
  (:use #:cl))
(in-package #:queue/struct)

(defstruct queue
  (head nil)
  (tail nil))

(defparameter *q* (make-queue))

(defmethod enqueue ((queue queue) item)
  (let ((item (list item)))
    (if (queue-head queue)
        (rplacd (queue-tail queue) item)
        (setf (queue-head queue) item))
    (setf (queue-tail queue) item)))

(defmethod dequeue ((queue queue))
  (when (queue-head queue)
    (setf (queue-head queue)
          (cdr (queue-head queue)))))


;; CLOS approach

(defpackage #:queue/clos
  (:use #:cl))
(in-package #:queue/clos)

(defclass queue ()
  ((head :initform nil :reader head :writer head!)
   (tail :initform nil :reader tail :writer tail!)))

(defmethod show ((queue queue))
  (values (head queue) (tail queue)))

(defmethod enqueue ((queue queue) item)
  (let ((item (list item)))
    (if (head queue)
        (rplacd (tail queue) item)
        (head! item queue))
    (tail! item queue)))
  
(defmethod dequeue ((queue queue))
  (when (head queue)
    (head! (cdr (head queue)) queue)))
