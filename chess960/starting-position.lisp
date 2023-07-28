#|
Generate Chess960 starting position
https://rosettacode.org/wiki/Generate_Chess960_starting_position

The purpose of this task is to write a program that can randomly generate any
one of the 960 Chess960 initial positions. You will show the result as the
first rank displayed with Chess symbols in Unicode: ♔♕♖♗♘ or with the letters
King Queen Rook Bishop kNight. 
|#

(defpackage #:chess960/start-position
  (:use #:cl))
(in-package #:chess960/start-position)
            
(defun fresh-board ()
  (vector 0 1 2 3 4 5 6 7))

(defun free-spaces (board)
  (remove-if-not 'numberp board))

(defun unicode-piece (p)
  (case p
    (b #\white_chess_bishop)
    (n #\white_chess_knight)
    (q #\white_chess_queen)
    (r #\white_chess_rook)
    (k #\white_chess_king)))

(defun place-bishops (board pos1 pos2)
  (let ((index1 (1+ (* 2 pos1)))
        (index2 (* 2 pos2)))
    (setf (aref board index1) 'b)
    (setf (aref board index2) 'b)))
  
(defun place-queen (board pos)
  (let* ((free (free-spaces board))
         (index (aref free pos)))
    (setf (aref board index) 'q)))

(defun place-knights (board pos)
  (let* ((free (free-spaces board))
         (nn (case pos
               (0 '(0 . 1))
               (1 '(0 . 2))
               (2 '(0 . 3))
               (3 '(0 . 4))
               (4 '(1 . 2))
               (5 '(1 . 3))
               (6 '(1 . 4))
               (7 '(2 . 3))
               (8 '(2 . 4))
               (9 '(3 . 4))))
    (index1 (aref free (car nn)))
    (index2 (aref free (cdr nn))))
  (setf (aref board index1) 'n)
  (setf (aref board index2) 'n)))

(defun place-rooks (board)
  (let* ((free (free-spaces board))
         (index1 (aref free 0))
         (index2 (aref free 2)))
    (setf (aref board index1) 'r)
    (setf (aref board index2) 'r)))

(defun place-king (board)
  (let* ((free (free-spaces board))
         (index (aref free 0)))
    (setf (aref board index) 'k)))


(defun generate (&optional (n (random 959)))
  (let ((board (fresh-board)))
    (multiple-value-bind (n2 b1) (floor n 4)
      (multiple-value-bind (n3 b2) (floor n2 4)
        (multiple-value-bind (n4 q) (floor n3 6)
          (format nil "n ~a~%n2 ~a, b1 ~a~%n3 ~a, b2 ~a~%n4 ~a, q ~a~%"
                  n n2 b1 n3 b2 n4 q)
          (place-bishops board b1 b2)
          (place-queen board q)
          (place-knights board n4)
          (place-rooks board)
          (place-king board)
          board)))))

(defun print-board (board &optional (transform #'identity))
  (format t "~{~a~^ ~}~%"
          (map 'list transform board)))

(princ "Generate random: ")
(print-board (generate) #'unicode-piece)

(princ "Generate spid 518: ")
(print-board (generate 518) #'unicode-piece)
