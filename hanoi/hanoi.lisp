;; tower of hanoi
(defun move (n from to spare)
  (if (= n 0) "done"
      (progn
        (move (1- n) from spare to)
        (format t "moving ~d from ~d to ~d with ~d as spare~%" n from to spare)
        (move (1- n) spare to from))))


(move 3 1 2 3)
