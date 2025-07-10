(ql:quickload "green-threads")

;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

;; TODO create state to avoid rendering the entire board every time
;; TODO create separate threads for gameloop and user input collection

(defpackage :fg-snake
  (:use :cl))

(in-package :fg-snake)

(defparameter *size-x* 20)
(defparameter *size-y* 20)


;; snake example
(defparameter *snake* nil)


(define-symbol-macro head (car *snake*))
(define-symbol-macro neck (cadr *snake*))

(define-symbol-macro at-north-edge (zerop (cdr head)))
(define-symbol-macro at-south-edge (= (1- *size-y*) (cdr head)))
(define-symbol-macro at-west-edge (zerop (car head)))
(define-symbol-macro at-east-edge (= (1- *size-x*) (car head)))

(defparameter *moves*
  '((:north 0 . -1)
    (:south 0 . 1)
    (:east 1 . 0)
    (:west -1 . 0)))

(defun reset ()
  (let ((x (random *size-x*))
        (y (random *size-y*)))
    (setf *snake* (list (cons x y)))))


(defun copy-head ()
  (copy-list head))


(defun direction-data (direction)
  (cdr (assoc direction *moves*)))


(defun move-head (x y)
  (incf (car head) x)
  (incf (cdr head) y))


(defun tail-to-head ()
  (setf *snake* (cons (copy-head) (butlast *snake*))))


(defun move (direction &key (grow nil))
  (destructuring-bind (x . y) (direction-data direction)
    (if grow
        (push (copy-head) *snake*)
        (when neck (tail-to-head)))
    (move-head x y))
  *snake*)


(defun grow (direction) (move direction :grow t))
  

(defun snake-collision-p (x y)
  (let ((new-head (cons (+ (car head) x)
                        (+ (cdr head) y))))
    (find new-head *snake* :test #'equalp)))


(defun can-move-p (direction)
  (destructuring-bind (x . y) (direction-data direction)
    (not (or (and at-east-edge (= x 1))
             (and at-west-edge (= x -1))
             (and at-north-edge (= y -1))
             (and at-south-edge (= y 1))
             (snake-collision-p x y)))))


(defun draw-board ()
  (let ((board (make-array (list *size-x* *size-y*) :initial-element #\.)))
    (loop for (x . y) in *snake*
          and h = t then nil
          do (setf (aref board x y) (if h #\H #\S)))
    (loop for y below *size-y*
          do (loop for x below *size-x*
                   do (princ (aref board x y))
                   finally (terpri)))))


(defun get-input ())

(defun main ()
  (loop
    for input = (get-input)
    for board = (draw-board)
    do )
          
