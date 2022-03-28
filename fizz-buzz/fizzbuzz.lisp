
(defun fizz-buzz (n)
  (let
      ((fizz (mod n 3)) (buzz (mod n 5)))
    (format nil "~[Fizz~:;~]~[Buzz~:;~]~[~:;~a~]" fizz buzz (* fizz buzz) n)))



(defun fizz-buzz-loop ()
  (loop for n from 1 to 100
        and fizz = (mod n 3)
        and buzz = (mod n 5)
        collect (format nil "~[Fizz~:;~]~[Buzz~:;~]~[~:;~a~]" fizz buzz (* fizz buzz) n)))
