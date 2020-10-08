#!/usr/bin/sbcl --script

(defparameter *graph* '((a b . 7) (a c . 9) (a f . 14)
                        (b c . 10) (b d . 15)
                        (c d . 11) (c f . 2)
                        (d e . 6)
                        (e f . 9)))

(defparameter *unknown* most-positive-word)

(defstruct node
  (name nil :type symbol)
  (cost nil :type number)
  (from nil :type symbol))

(defun create-nodes-table (start)
  (mapcar (lambda (it)
            (make-node :name it :cost (if (eq it start) 0 *unknown*) :from nil))
          (remove-duplicates
           (mapcan (lambda (it) (list (first it) (second it))) *graph*))))

(defun neighbors (node)
  (mapcan (lambda (it)
            (cond
              ((eq (first it) node) `((,(second it) . ,(cddr it))))
              ((eq (second it) node) `((,(first it) . ,(cddr it))))))
          *graph*))

(defun dijkstra (start finish)
  (let ((nodes-table (create-nodes-table start))
        (results nil)
        (node nil)
        (neighbor-node nil))
    (loop
       while nodes-table
       do
         (progn
           (setq nodes-table (stable-sort nodes-table #'< :key #'node-cost))
           (setq node (pop nodes-table))
           (push node results)

           (loop
              with neighbors = (neighbors (node-name node))
              with new-cost = nil
              for neighbor in neighbors
              do
                (progn
                  (setq neighbor-node (find (car neighbor) nodes-table :key #'node-name))
                  (when neighbor-node
                    (setq new-cost (+ (node-cost node) (cdr neighbor)))
                    (when (< new-cost (node-cost neighbor-node))
                      (progn
                        (setf (node-cost neighbor-node) new-cost)
                        (setf (node-from neighbor-node) (node-name node))))))))
       finally (return results))))

(defun shortest-path (start finish)
  (let ((results (dijkstra start finish))
        (node finish)
        (path nil))
     (loop
        for item = (find node results :key #'node-name)
        do (progn
             (unless path (setq path (cons node (node-cost item))))
             (setq node (node-from item))
             (when node (setq path (cons node path))))
        while node
        finally (return path))))

(let ((start (read-from-string (second *posix-argv*))) (finish (read-from-string (third *posix-argv*))))
  (format t "~a~%" (shortest-path start finish)))

