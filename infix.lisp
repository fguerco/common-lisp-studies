(ql:quickload '(:fg-utils :parachute))

(defpackage :infix
  (:use :cl :fg :parachute))

(in-package :infix)

(defun calculate (expr)
  (labels
      ((decompose (op expr)
         (if (and (consp expr) (cddr expr))
             (destructuring-bind (a b c . d) expr
               (if (find b op)
                   (decompose op (cons (funcall b (calculate a) (calculate c)) d))
                   (cons a (decompose op (cdr expr)))))
             expr)))
    (if (numberp expr)
        expr
        (let ((r (->> expr
                   (decompose '(* /))
                   (decompose '(+ -)))))
          (if (consp r) (car r) r)))))

(defun parse-number (st)
  (read-from-string
   (with-output-to-string (out)
     (loop for c = (peek-char nil st nil :eof)
           while (and (characterp c) (or (digit-char-p c) (eql #\. c)))
           do (princ (read-char st) out)))))

(defun minus-as-operator-p (ch prev)
  (and (characterp prev)
       (eql #\- ch)
       (or (digit-char-p prev)
           (eql #\( prev))))

(defun read-terms (st)
  (do (result
       (c #1=(peek-char t st nil :eof) #1#)
       (p nil c))
      ((find c '(#\) :eof)) (progn (read-char st nil) (nreverse result)))
    (cond ((eql #\( c) (read-char st) (push (read-terms st) result))
          ((digit-char-p c) (push (parse-number st) result))
          ((or (find c "+*/") (minus-as-operator-p c p))
           (push (find-symbol (string (read-char st))) result))
          ((not (minus-as-operator-p c p)) (read-char st) (push :invert result))
          (t (read-char st)))))

(defun invert-signals (expr)
  (reduce (lambda (it prev)
            (cond ((consp it) (cons (invert-signals it) prev))
                  ((and (eql :invert it) (atom (car prev))) `(-1 * ,@prev))
                  ((and (eql :invert it) (consp (car prev)))
                   `((0 - ,(car prev))))
                  (t (cons it prev))))
          expr :from-end t :initial-value nil))

(defun normalize-list (expr)
  (cond ((atom expr) (list expr))
        ((and (consp expr) (null (cdr expr))) (car expr))
        (t expr)))

(defun infix (expr)
  (with-input-from-string (st expr)
    (-> st
      read-terms
      invert-signals
      normalize-list
      calculate)))

;;;; tests

(defmacro make-assertions (test-data)
  `(progn
     ,@(loop for (expr expected) in test-data
             collect `(is = ,expected (infix ,expr)))))
                

(define-test infix-tests ()
  (make-assertions (("-123" -123)
                    ("12 * -1" -12)
                    ("12*-1" -12)
                    ("1 - 1" 0)
                    ("1-1" 0)
                    ("1 -1" 0)
                    ("4 - 1" 3)
                    ("4-1" 3)
                    ("1 - -1" 2)
                    ("1- -1" 2)
                    ("1 + 2 - 3 * (4 / 6)" 1)
                    ("(1 + 2) - 3 * (4 / 6)" 1)
                    ("12 * 123 / (-5 + 2)" -492)
                    ("12* 123/(-5 + 2)" -492)
                    ("(1 + 2 + 3) * 10 / ((2 * 3) + 3)" 20/3)
                    ("((1 + 2) - 3) * (4 / 6)" 0)
                    ("((1 + 2) * (1 - (1 + 2))) + (((10 - 1) * 2) + 3)" 15)
                    ("2 + 3" 5)
                    ("2 + 3 / 4" 11/4)
                    ("2 * 3 - 4" 2)
                    ("2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10" 7000)
                    ("2 * -3 - -4 + -0.25" -2.25)
                    ("13 + 112" 125)
                    ("325 + 1124" 1449)
                    ("5 + 11 - 4" 12)
                    ("5 * 11 - 20 + 5" 40)
                    ("1 + 2 - 3 * (4 / 6)" 1)
                    ("(1 + 2) - 3 * (4 / 6)" 1)
                    ("((1 + 2) - 3) * (4 / 6)" 0)
                    ("12* 123/-(-5 + 2)" 492)
                    ("(-5 + 2)" -3)
                    ("-(-5 + 2)" 3)
                    ("(3 + 2)" 5)
                    ("(3 + 5) * 10" 80))))

(test :infix)

