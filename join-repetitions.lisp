(defpackage #:join-rep
  (:use :cl))

(in-package #:join-rep)

(defun join-repetitions (string)
  "Join repetitions and exclude non repeating characters using a hash table"
  (let* ((size (length string))
         (ht (make-hash-table :size size)))
    (map nil (lambda (x)
               (incf (gethash x ht 0)))
         string)
    (with-output-to-string (st)
      (maphash (lambda (k v)
                 (when (> v 1)
                   (dotimes (i v)
                     (write-char k st))))
               ht))))

(defun join-repetitions* (string)
  "Join repetitions and exclude non repeating characters sorting the string"
  (let ((string (sort string #'char<))
        (prev-char nil)
        (last-char nil))
    (with-output-to-string (st)
      (flet ((process-char (c)
               (cond
               ((eql c prev-char)
                (when last-char
                  (write-char c st)
                  (setq last-char nil))
                (write-char c st))
               (t (setq last-char c)))
               (setq prev-char c)))
      (map nil #'process-char string)))))
