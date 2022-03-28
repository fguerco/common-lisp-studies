
(defvar *test-name* nil)
(defvar *test-depth* -1)

(defun format-indented (destination control-string &rest format-arguments)
  (let ((padded-control-string (format nil "~v@{~a~:*~}~*~a" *test-depth* "   " control-string)))
    (apply #'format destination padded-control-string format-arguments)))

(defun report-result (result form)
  (let ((*test-depth* (1+ *test-depth*)))
    (format-indented t "~:[✗~;✓~] ... ~a~%" result form)
    result))

(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for form in forms collect `(unless ,form (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for form in forms collect `(report-result ,form ',form))))


(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name)
           (*test-depth* (1+ *test-depth*)))
       (format-indented t "~a:~%" *test-name*)
       ,@body)))


(deftest test-+ ()
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -5)))

(deftest test-* ()
  (check
    (= (* 1 2) 2)
    (= (* 2 4) 8)))


(deftest test-arithmetic ()
   (test-+)
   (test-*))


(deftest test-math ()
  (test-arithmetic))


(test-math)
