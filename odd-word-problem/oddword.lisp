#!/usr/bin/sbcl --script
#|
        Odd-word problem   -- http://wiki.c2.com/?OddWordProblem
        ========================================================

Consider a character set consisting of letters, a space, and a point. Words consist
of one or more, but at most 20 letters. An input text consists of one or more words
separated from each other by one or more spaces and terminated by 0 or more spaces
followed by a point. Input should be read from, and including, the first letter of
the first word, up to and including the terminating point. The output text is to be
produced such that successive words are separated by a single space with the last
word being terminated by a single point. Odd words are copied in reverse order while
even words are merely echoed. For example, the input string

  : whats the matter with kansas.


becomes

  : whats eht matter htiw kansas.


The problem is further restricted in that the characters must be read and printed one at a time. 
|#


(defparameter *out* nil)
(defparameter *in* nil)

(defun peek-ch (&optional (ignore-whitespace)) (peek-char ignore-whitespace *in*))
(defun read-ch () (read-char *in*))
(defun write-ch (c) (when c (write-char c *out*)))
(defun is-word-separator (&optional (c (peek-ch))) (position c #(#\Space #\.)))
(defun write-next () (write-ch (read-ch)))

(defun spaces (&optional start)
  (loop for c  = (peek-ch start) while (eql c #\Space) do (read-ch)
     finally (unless (or start (eql c #\.)) (write-ch #\Space))))

(defun even-word ()
  (unless (is-word-separator)
      (progn (write-next)
             (even-word))))

(defun odd-word ()
  (unless (is-word-separator)
      (let ((ch (read-ch)))
        (odd-word)
        (write-ch ch))))
         
(defun odd-word-problem (sentence)
  (with-output-to-string (*out*)
    (with-input-from-string (*in* sentence)
      (spaces t)
      (loop
         for c = (peek-ch)
         with word = -1
         do
           (cond
             ((eql c #\.) (write-next))
             ((eql c #\Space) (spaces))
             (t (if (evenp (incf word)) (even-word) (odd-word))))
         until (eql c #\.)))))

(defun test (sentence expected)
  (let ((got (odd-word-problem sentence)))
    (if (string= expected got)
        (format t "~&passed~%")
        (format t "~&FAILED. Expected '~a' got '~a'~%" expected got))))

(test "     what is the meaning of life."
      "what si the gninaem of efil.")

(test "whats    the     matter     with    kansas       ."
      "whats eht matter htiw kansas.")

(test "we are not in kansas any more.    should not read this"
      "we era not ni kansas yna more.")
