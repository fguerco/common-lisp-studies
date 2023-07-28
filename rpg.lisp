#|
RPG attributes generator
Write a program that:

    Generates 4 random, whole values between 1 and 6.
    Saves the sum of the 3 largest values.
    Generates a total of 6 values this way.
    Displays the total, and all 6 values once finished.

    The order in which each value was generated must be preserved.
    The total of all 6 values must be at least 75.
    At least 2 of the values must be 15 or more.
|#

(defpackage #:rpg-chargen
  (:use #:cl))

(in-package #:rpg-chargen)

(defvar attribute-names '(str dex con int wis cha))
(defvar dices-to-roll '(6 6 6 6))

(defun random-attribute ()
  (apply #'+ (cdr (sort (mapcar (lambda (y) (1+ (random y))) dices-to-roll)
                        #'<))))

(defun roll-dice ()
  (mapcar (lambda (x)
            (declare (ignore x))
            (random-attribute)) attribute-names))

(defun validp (attributes)
  (and (>= (apply #'+ attributes) 75)
       (>= (count-if (lambda (x) (>= x 15)) attributes) 2)))

(defun new-char ()
  (let ((char (roll-dice)))
    (cond
      ((validp char) (mapcar #'cons attribute-names char))
      (t (new-char)))))

