(defparameter *graph* '((a (a b . 7) (a c . 9) (a f . 14))
                        (b (b c . 10) (b d . 15))
                        (c (c d . 11) (c f . 2))
                        (d (d e . 6))
                        (e (e f . 9))))


(defun list-paths (start)
  (cdr (assoc start *graph*)))

(defun starts-with (path item)
  (eql item (first path)))

(defun ends-with (path item)
  (eql item (first (last path))))

(defun all-paths (start finish &optional (cost 0))
  (loop
     for section in (list-paths start)
     for next-node = (second section)
     for path = (if (and (not (eql start finish)) (list-paths next-node))
                    (cons start (first (all-paths next-node finish (+ cost (cddr section)))))
                    (cons start cost))
     if (and (starts-with path start) (ends-with path finish))
     collect path))

(defun dijkstra-shortest-path (start finish)
  (flet ((path-cost (x) (cdr (last x))))
    (first (sort (all-paths start finish) #'< :key #'path-cost))))
