#!/usr/bin/sbcl --script

(defparameter *in* nil)
(defparameter *out* nil)

(defmacro setup (sentence buffer-var &body forms)
  `(with-output-to-string (*out*)
     (with-input-from-string (*in* ,sentence)
       (let ((,buffer-var (make-array 20 :fill-pointer 0)))
         ,@forms))))
  

(defun skip-spaces (&optional print)
  (loop for c = (peek-char nil *in*)
     while (eql c #\Space)
     do (read-char *out*)
     finally (if print (write-char #\Space *out*))))


(defun print-bufer

(defun odd-word-problem (sentence)
  (setup sentence buffer
    (loop for c = (peek-char t *in*)
         (cond
           (eql c #\.)
           ((buffer-char c) (vector-push (read-charc buffer)))
             
