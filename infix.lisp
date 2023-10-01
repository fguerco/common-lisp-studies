(ql:quickload '("fg-utils" "parachute"))

(defpackage :infix
  (:use :cl :fg :parachute))

(in-package :infix)

(defstruct expr op a b)

(defmethod evaluate ((expr expr))
  (with-slots (op a b) expr
    (funcall op (evaluate a) (evaluate b))))

(defmethod evaluate ((expr number))
  expr)

(defmethod evaluate ((expr cons))
  (evaluate (car expr)))

(defun to-expr (op tokens)
  (if (and (listp tokens) (cddr tokens))
      (destructuring-bind (x y z . r) tokens
        (if (find y op)
            (as-> (it (make-expr :op y :a (compose x) :b (compose z)))
              (cons it r)
              (to-expr op it))
            (cons x (to-expr op (cdr tokens)))))
      tokens))


(defun compose (tokenized)
  (->> tokenized
    (to-expr '(* /))
    (to-expr '(+ -))))

(defun invert-signals (expr)
  (labels ((inv (expr)
             (if (atom expr)
                 expr
                 (destructuring-bind (a &optional b . r) expr
                   (if (eql a :invert)
                       (cons `(-1 * ,(inv b)) (inv r))
                       (cons (inv a) (inv (cdr expr))))))))
    (pop-single (inv expr))))

(defun parse-number (st)
  (read-from-string
   (with-output-to-string (out)
     (loop for c = (peek-char nil st nil :eof)
           while (and (characterp c) (or (digit-char-p c) (eql #\. c)))
           do (princ (read-char st) out)))))

(defun minus-operator-p (ch prev)
  (and (characterp prev)
       (eql #\- ch)
       (or (digit-char-p prev)
           (eql #\( prev))))

(defun operator-p (ch prev)
  (or (find ch "+*/")
      (minus-operator-p ch prev)))

(defun tokenize (st)
  (flet ((peek () (peek-char t st nil :eof))
         (consume () (read-char st nil)))
    (loop
      for c = (peek)
      and p = nil then c
      until (find c '(#\) :eof))
      if (eql #\( c)
        collect (progn (consume) (tokenize st))
      else
        if (digit-char-p c)
          collect (parse-number st)
      else
        if (operator-p c p)
          collect (join-to-symbol (consume))
      else
        unless (minus-operator-p c p)
          collect (progn (consume) :invert)
      else do (consume)
      finally (consume))))

(defun infix (expr)
  (with-input-from-string (st expr)
    (-> st
      tokenize
      invert-signals
      compose
      evaluate)))

;;;; tests

(defparameter *test-data*
  #(("-123" -123)
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
    ("(3 + 5) * 10" 80)))

(defmacro make-assertions ()
  `(progn
     ,@(loop for (expr expected) across *test-data*
             collect `(is = ,expected (infix ,expr)))))

(define-test infix-tests ()
  (make-assertions))

(defun stress-test (how-many)
  (let* ((len (length *test-data*))
         (exprs (loop repeat how-many
                      for (e r) = (elt *test-data* (random len))
                      collect e)))
    (time
     (map nil #'infix exprs))))

(test :infix)
