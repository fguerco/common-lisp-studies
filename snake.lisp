(ql:quickload '("green-threads" "cl-tui"))

;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

;; TODO create state to avoid rendering the entire board every time
;; TODO create separate threads for gameloop and user input collection

(defpackage :fg-snake
  (:use :cl))

(in-package :fg-snake)

(defparameter *size-x* 10)
(defparameter *size-y* 10)


;; snake example
(defparameter *snake* nil)
(defparameter *food* nil)


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

(defun generate-place ()
  (let ((x (random *size-x*))
        (y (random *size-y*)))
    (cons x y)))


(defun spawn-food ()
  (loop for food = (generate-place)
        while (find food *snake* :test #'equalp)
        finally (return (setf *food* food))))

(defun reset ()
  (setf *snake* (list (generate-place)))
  (spawn-food))


(defun copy-head ()
  (copy-list head))


(defun direction-data (direction)
  (cdr (assoc direction *moves*)))


(defun move-head (x y)
  (setf (car head) x)
  (setf (cdr head) y))


(defun tail-to-head ()
  (setf *snake* (cons (copy-head) (butlast *snake*))))


(defun next-position (direction)
  (destructuring-bind (x . y) (direction-data direction)
    (cons (+ (car head) x)
          (+ (cdr head) y))))


(defun move (direction)
  (let* ((next (next-position direction))
         (x (car next))
         (y (cdr next))
         (grow (equalp next *food*)))
    (if grow
        (push next *snake*)
        (when neck (tail-to-head)))
    (move-head x y)
  (when grow (spawn-food)))
  *snake*)



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

(defun on-direction-chosen (direction)
   (move direction)
  (draw-board))


(defun draw-board ()
  (terpri)
  (let ((board (make-array (list *size-x* *size-y*) :initial-element #\.)))
    (loop for (x . y) in *snake*
          and h = t then nil
          do (setf (aref board x y) (if h #\H #\S)))
    (when *food*
      (destructuring-bind (fx . fy) *food*
        (setf (aref board fx fy) #\F)))
    (loop for y below *size-y*
          do (loop for x below *size-x*
                   do (princ (aref board x y))
                   finally (terpri)))))


(defun get-input ()
  (format t "Enter direction: ")
  (case (elt (read-line) 0)
    ((#\n #\8) :north)
    ((#\s #\2) :south)
    ((#\e #\6) :east)
    ((#\w #\4) :west)))
    

(defun main ()
  (reset)
  (draw-board)
  (loop
    for i from 1
    for input = (get-input)
    do (format t "input: ~a~%" input)
    if input
      do (on-direction-chosen input)
    else
      do (return)))
     
(main)
