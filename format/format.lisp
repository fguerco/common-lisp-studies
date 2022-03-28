
(loop for cons on '(1 2 3 4 5)
      do (format t "~a" (car cons))
      when (cdr cons) do (format t ", "))

(loop for s being the external-symbols of "COMMON-LISP" collect s)




(defun deftag (name attributes &rest contents)
  (with-output-to-string (out)
    (format out "<~a~{~^ ~a=\"~a\"~}>~%" name attributes)
    (format out "  ~{~a~}~%" contents)
    (format out "</~a>~%" name)))



(format
 t "~a"
 (deftag 'html ()
   (deftag 'head ())
   (deftag 'body ()
   (deftag 'a '(:href "http://site.com" :target "_blank")
     (deftag 'p '(:class "p1")
       "Clique aqui"
       (deftag 'span () "Outra linha"))
     (deftag 'p '(:class "p2") "Outro paragrafo")))))


(eval
 '(deftag 'span () "Clique aqui"))
