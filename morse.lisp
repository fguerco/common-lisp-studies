(ql:quickload "parachute")

(defpackage :morse
  (:use :cl :parachute))

(in-package :morse)

(defvar *table*
  '(
    (#\A . ".-")   (#\B . "-...") (#\C . "-.-.")
    (#\D . "-..")  (#\E . ".")    (#\F . "..-.")
    (#\G . "--.")  (#\H . "....") (#\I . "..")
    (#\J . ".---") (#\K . "-.-")  (#\L . ".-..")
    (#\M . "--")   (#\N . "-.")   (#\O . "---")
    (#\P . ".--.") (#\Q . "--.-") (#\R . ".-.")
    (#\S . "...")  (#\T . "-")    (#\U . "..-")
    (#\V . "...-") (#\W . ".--")  (#\X . "-..-")
    (#\Y . "-.--") (#\Z . "--..")

    (#\0 . "-----") (#\1 . ".----") (#\2 . "..---")
    (#\3 . "...--") (#\4 . "....-") (#\5 . ".....")
    (#\6 . "-....") (#\7 . "--...") (#\8 . "---..")
    (#\9 . "----.")

    (#\. . ".-.-.-") (#\, . "--..--")  (#\? . "..--..")
    (#\' . ".----.") (#\! . "-.-.--")  (#\/ . "-..-.")
    (#\( . "-.--.")  (#\) . "-.--.-")  (#\& . ".-...")
    (#\: . "---...") (#\; . "-.-.-.")  (#\= . "-...-")
    (#\+ . ".-.-.")  (#\- . "-....-")  (#\_ . "..--.-")
    (#\" . ".-..-.") (#\$ . "...-..-") (#\@ . ".--.-.")
    (#\¿ . "..-.-")  (#\¡ . "--...-")))

(defun char->morse (char) (cdr (assoc char *table*)))
(defun morse->char (morse) (car (rassoc  morse *table* :test #'equal)))

(defun encode (text)
  (loop for string in (uiop:split-string (string-upcase text)
                                         :separator '(#\space #\tab #\newline))
        for word = (map 'list #'char->morse string)
        when word collect word into result
        finally (return (format nil "~{~{~a~^ ~}~^ / ~}" result))))

(defun decode (morse)
  (loop for letter in (uiop:split-string morse)
        for char = (morse->char letter)
        collect (or char #\space) into result
        finally (return (coerce result 'string))))


(define-test test-encode
  (is equal "... --- ..." (encode "sos"))
  (is equal "- .... .. ... / .. ... / .- / - . ... -"
      (encode "this is a test")))

(define-test test-decode ()
  (is equal "SOS" (decode "... --- ..."))
  (is equal "THIS IS A TEST"
      (decode "- .... .. ... / .. ... / .- / - . ... -")))


(test :morse)
