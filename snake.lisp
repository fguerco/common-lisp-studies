(ql:quickload '("bordeaux-threads" "cl-tui") :silent t)

;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

;; TODO create state to avoid rendering the entire board every time

(defpackage :fg-snake
  (:use :cl)
  (:use :cl-tui)
  (:use :bt2))

(in-package :fg-snake)

(defparameter *size-x* 10)
(defparameter *size-y* 10)

(defparameter *tick* 1.0)
(defparameter *difficulty* 0.85)

(defparameter *board* nil)

(defparameter *direction* nil)

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

(defun increase-speed ()
  (when (zerop (mod (length *snake*) 5))
    (setf *tick* (* *tick* 0.85))))

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
    (when grow
      (spawn-food)
      (increase-speed)))
  *snake*)



(defun snake-collision-p (x y)
  (let ((new-head (cons (+ (car head) x)
                        (+ (cdr head) y))))
    (find new-head *snake* :test #'equalp)))


(defun valid-move-p (direction)
  (destructuring-bind (x . y) (direction-data direction)
    (not (or (and at-east-edge (= x 1))
             (and at-west-edge (= x -1))
             (and at-north-edge (= y -1))
             (and at-south-edge (= y 1))
             (snake-collision-p x y)))))

(defun random-item (seq)
  (elt seq (random (length seq))))

(defun pick-direction ()
  (loop for dir = (car (random-item *moves*))
        until (valid-move-p dir)
        finally (return dir)))


(defun reset ()
  (setf *snake* (list (generate-place)))
  (setf *direction* (pick-direction))
  (spawn-food))


(defun draw-board (&key frame)
  (let ((board (make-array (list *size-x* *size-y*) :initial-element #\.)))
    (loop for (x . y) in *snake*
          and h = t then nil
          do (setf (aref board x y) (if h #\H #\S)))
    (destructuring-bind (fx . fy) *food*
      (setf (aref board fx fy) #\F))
    (loop for y below *size-y*
          do (loop for x below *size-x*
                   do (put-char frame y x (aref board x y))))))


(defun on-direction-chosen (direction)
  (auto-walk :stop t)
  (setf *direction* direction)
  (refresh)
  (auto-walk))


(defun get-input ()
  (let ((key (read-key)))
    (case key
      ((:key-up #\w #\8) :north)
      ((:key-down #\s #\2) :south)
      ((:key-right #\d #\6) :east)
      ((:key-left #\a #\4) :west))))


;; thread 1: auto run
(let (thread)
  (defun auto-walk (&key stop)
    (when (and (threadp thread) (thread-alive-p thread))
      (destroy-thread thread)
      (setf thread nil))
    (unless stop
      (setf thread
            (make-thread
             (lambda ()
               (loop
                 (move *direction*)
                 (refresh)
                 (sleep *tick*))))))))


;; thread 2: read key
(let (thread)
  (defun collect-iput ()
    (setf thread
          (make-thread
           (lambda ()
             (loop
               for input = (get-input)
               do (case input
                    (:quit (return))
                    (:refresh (reset))
                    ((:north :south :east :west) (on-direction-chosen input)))))))))



(defun game-over ()
  (auto-walk :stop t))

(define-frame callback (simple-frame :render 'draw-board) :on :root)

(defun main ()
  (reset)
  (with-screen ()
    (refresh)
    (collect-iput)
    (auto-walk)
    (loop)))
      
    ;;  for input = (get-input)
    ;;  do (case input
    ;;       (:quit (return))
    ;;       (:refresh (reset))
    ;;       ((:north :south :east :west) (on-direction-chosen input))))))
     
(main)
