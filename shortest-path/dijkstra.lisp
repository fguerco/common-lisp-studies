#!/usr/bin/sbcl --script

(defparameter *graph* '((a b . 7) (a c . 9) (a f . 14)
                        (b c . 10) (b d . 15)
                        (c d . 11) (c f . 2)
                        (d e . 6)
                        (e f . 9)))

(defparameter *unknown* most-positive-word)

(defun create-nodes-table (start)
  (mapcar (lambda (it)
            (list :node it :cost (if (eq it start) 0 *unknown*) :from nil))
          (remove-duplicates
           (mapcan (lambda (it) (list (first it) (second it))) *graph*))))

(defun neighbors (node)
  (mapcar (lambda (it)
            (cons (if (eq (first it) node)
                      (second it)
                      (first it))
                  (cddr it)))
          (remove-if-not (lambda (it) (or (eq (first it) node)
                                          (eq (second it) node)))
                         *graph*)))

(defun dijkstra (start finish)
  (null finish)
  (let ((nodes-table (create-nodes-table start))
        (results nil)
        (node nil)
        (neighbor-node nil))
    (loop
       while nodes-table
       do
         (progn
           (setq nodes-table (stable-sort nodes-table #'< :key #'fourth))
           (setq node (pop nodes-table))
           (push node results)

           (loop
              with neighbors = (neighbors (getf node :node))
              with new-cost = nil
              for neighbor in neighbors
              do
                (progn
                  (setq neighbor-node (find (car neighbor) nodes-table :key #'second))
                  (when neighbor-node
                    (setq new-cost (+ (getf node :cost) (cdr neighbor)))
                    (when (< new-cost (getf neighbor-node :cost))
                      (progn
                        (setf (getf neighbor-node :cost) new-cost)
                        (setf (getf neighbor-node :from) (getf node :node))))))))
       finally (return results))))

(defun shortest-path (start finish)
  (let ((results (dijkstra start finish))
        (node finish)
        (path nil))
     (loop
        for item = (find node results :key #'second)
        do (progn
             (unless path (setq path (cons node (getf item :cost))))
             (setq node (getf item :from))
             (when node (setq path (cons node path))))
        while node
        finally (return path))))

(let ((start (read-from-string (second *posix-argv*))) (finish (read-from-string (third *posix-argv*))))
  (format t "~a~%" (shortest-path start finish)))
