(defvar *roman-table* '((1000 . M) (900 . CM) (500 . D) (400 . CD)
                        (100 . C) (90 . XC) (50 . L) (40 . XL)
                        (10 . X) (9 . IX) (5 . V) (4 . IV)
                        (1 . I)))


(defun roman (number)
  (when (>= number 4000) (error "Maximum allowed is 3999"))
  (labels ((f (n table)
             (cond
               ((>= n (caar table)) (cons (cdar table) (f (- n (caar table)) table)))
               ((> n 0) (f n (cdr table)))
               (t nil))))
    (format nil "~{~a~}" (f number *roman-table*))))


(deftest test-roman ()
  (check
    (equal "MMMCMXCIX" (roman 3999))
    (equal "MMII" (roman 2002))
    (equal "MCMLXXXIII" (roman 1983))))


