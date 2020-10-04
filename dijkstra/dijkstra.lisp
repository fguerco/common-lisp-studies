#!/usr/bin/sbcl --script

(defparameter *graph* '((a (a b . 7) (a c . 9) (a f . 14))
                        (b (b c . 10) (b d . 15))
                        (c (c d . 11) (c f . 2))
                        (d (d e . 6))
                        (e (e f . 9))))


(defun all-paths (start finish &optional (cost 0))
  (loop
     for section in (cdr (assoc start *graph*))
     for next-node = (second section)
     for next-cost = (+ cost (cddr section))
     for path = (if (eql next-node finish)
                    (cons start (cons finish next-cost))
                    (let ((rest-of-path (first (all-paths next-node finish next-cost))))
                      (when rest-of-path (cons start rest-of-path))))
     if path collect path))


(defun dijkstra-shortest-path (start finish)
  (first (sort (all-paths start finish) #'< :key (lambda (x) (cdr (last x))))))


(let ((start (second *posix-argv*))
      (finish (third *posix-argv*)))
  (when (and start finish)
    (format t "~a~%" (dijkstra-shortest-path (read-from-string start) (read-from-string finish)))))
