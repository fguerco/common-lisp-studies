(defparameter *graph* '((a (a b . 7) (a c . 9) (a f . 14))
                        (b (b c . 10) (b d . 15))
                        (c (c d . 11) (c f . 2))
                        (d (d e . 6))
                        (e (e f . 9))))


(defun list-paths (start)
  (cdr (assoc start *graph*)))

(defun starts-with (path item)
  (eql item (caar path)))

(defun ends-with (path item)
  (eql item (cadar (last path))))

(defun total-cost (path)
  (reduce (lambda (prev it)
            (+ prev (cddr it)))
          path :initial-value 0))

(defun all-paths (start finish)
  (loop
     for node in (list-paths start)
     for next-node = (second node)
     for path = (if (list-paths next-node)
                    (cons node (first (all-paths next-node finish)))
                    (cons node nil))
     if (and (starts-with path start) (ends-with path finish))
     collect path))

(defun shortest-path (start finish)
  (car (sort (all-paths start finish)
        (lambda (a b)
          (< (total-cost a) (total-cost b))))))
