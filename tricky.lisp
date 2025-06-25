;; input: "3[a]2[bc]"
;; output: "aaabcbc"

;; input: "3[a2[c]]]"
;; output: "accaccacc"

(defun peek (st) (peek-char nil st nil :eof))
(defun consume (st) (read-char st nil))


(defun extract-from-stream (st predicate)
  (with-output-to-string (out)
    (loop for c = (peek st)
          while (funcall predicate c)
          do (princ (read-char st) out))))


(defun parse-number (st)
  (parse-integer
   (extract-from-stream st #'digit-char-p)))


(defun read-chars (st)
  (flet ((predicate (c)
           (not (or (digit-char-p c) (eq c #\[) (eq c #\])))))
  (extract-from-stream st #'predicate)))


(defun tokenize (st)
  (loop for c = (peek st)
        until (find c (list #\] :eof))
        if (eq c #\[)
          collect (progn (consume st) (tokenize st))
        else
          if (digit-char-p c)
            collect (parse-number st)
        else
          collect (read-chars st)
        finally (consume st)))


(defun transform (data)
  (with-output-to-string (out)
    (loop
      for x in data
      and p = nil then x
      do (cond
           ((and (listp x) (integerp p))
            (format out "~v@{~a~:*~}" p (transform x)))
           ((stringp x) (princ x out))))))
      

(defun tricky (expr)
  (with-input-from-string (st expr)
    (transform (tokenize st))))




(defun test (input expected)
  (format t "Running test with input ~a..." input)
  (let* ((result (tricky input))
         (passed (equalp result expected)))
    (if passed
        (format t " PASSED~%")
        (format t " FAIL. Expected ~a got ~a~%" expected result))))

(let ((tests '(("3[a]2[bc]" . "aaabcbc")
               ("3[a2[c]]]" . "accaccacc")
               ("abc2[cde]fg3[ol4[a]]" . "abccdecdefgolaaaaolaaaaolaaaa")
               ("2[x2[y2[z2[a]]]]".  "xyzaazaayzaazaaxyzaazaayzaazaa"))))
  (loop for (val . expected) in tests
        do (test val expected)))

