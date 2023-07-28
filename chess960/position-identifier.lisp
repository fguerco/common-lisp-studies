#|
Find Chess960 starting position identifier
https://rosettacode.org/wiki/Find_Chess960_starting_position_identifier

This task is to go the other way: given a starting array of pieces (provided in
any form that suits your implementation, whether string or list or array, of
letters or Unicode chess symbols or enum values, etc.), derive its unique SP-ID.
For example, given the starting array QNRBBNKR (or ♕♘♖♗♗♘♔♖ or ♛♞♜♝♝♞♚♜),
which we assume is given as seen from White's side of the board from left to
right, your (sub)program should return 105; given the starting lineup of standard
chess, it should return 518.

You may assume the input is a valid Chess960 position; detecting invalid input
(including illegal characters or starting arrays with the bishops on the same
color square or the king not between the two rooks) is optional. 
|#

(defpackage #:chess960/spid
  (:use #:cl))
(in-package #:chess960/spid)

(defun find-knigts (board)
  (let* ((board (remove-if (lambda (x) (case x ((#\B #\Q) t))) board))
         (n1 (position #\N board))
         (n2 (position #\N board :from-end t)))
    (case (+ n2 (* 10 n1))
      (1 0)
      (2 1)
      (3 2)
      (4 3)
      (12 4)
      (13 5)
      (14 6)
      (23 7)
      (24 8)
      (34 9))))
    
(defun find-queen (board)
  (let ((board (remove #\B board)))
    (position #\Q board)))

(defun find-bishops (board)
  (let ((b1 (position #\B board))
        (b2 (position #\B board :from-end t)))
    (if (oddp b1)
        (values (/ (1- b1) 2) (/ b2 2))
        (values (/ (1- b2) 2) (/ b1 2)))))
    
(defun find-spid (board)
  (let* ((n4 (find-knigts board))
         (q (find-queen board))
         (n3 (+ q (* n4 6))))
    (multiple-value-bind (b1 b2) (find-bishops board)
      (let ((n2 (+ b2 (* n3 4))))
        (+ b1 (* n2 4))))))

(princ "spid for RNBQKBNR: ")
(princ (find-spid "RNBQKBNR"))
(fresh-line)

