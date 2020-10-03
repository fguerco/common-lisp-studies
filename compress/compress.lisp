;; if we use :from-end with reduce, lambda argument list is reversed too
(defun compress (items)
  "Compress a list replacing repeated sequential values for (<n> <value>)"
  (nreverse (reduce (lambda (prev it &aux (val (assoc it prev)))
                      (if val
                          (incf (cdr val))
                          (push (cons it 1) prev))
                      prev)
                    items
                    :initial-value nil)))

(compress '(1 2 1 2 1 2 3 4 4 4 4 4 5 5 6 7 6 6 6 6))

(compress "aabbcdcdcdcefghhijkijki")
