(ql:quickload "fg-utils")

(defpackage :infix
  (:use :cl)
  (:import-from :fg
                #:nlet #:pop-single #:-> #:->> #:as-> #:join-to-symbol))

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
    pop-single
    (to-expr '(* /))
    (to-expr '(+ -))))

(defun invert-signals (expr)
  (pop-single
   (nlet inv ((expr expr))
     (if (atom expr)
         expr
         (destructuring-bind (a &optional b . r) expr
           (if (eql a :invert)
               (cons `(-1 * ,(inv b)) (inv r))
               (cons (inv a) (inv (cdr expr)))))))))

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
  (let ((*read-default-float-format* 'double-float))
    (with-input-from-string (st expr)
      (-> st
        tokenize
        invert-signals
        compose
        evaluate))))
