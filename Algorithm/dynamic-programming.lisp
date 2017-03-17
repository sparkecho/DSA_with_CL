;; multistage graph problem

;; data structure: list of Edges, each Edge use a list to record the Start, End and Power
;; for example: AB is an edge, and it's length is 10, then it'll be present as (A B 10)
;; min cost of *graph* is: (1 3 6 10 12) or (1 2 7 10 12)
(defparameter *graph* '((1 2 9) (1 3 7) (1 4 3) (1 5 2)
                        (2 6 4) (2 7 2) (2 8 1)
                        (3 6 2) (3 7 7)
                        (4 8 11)
                        (5 7 11) (5 8 8)
                        (6 9 6) (6 10 5)
                        (7 9 4) (7 10 3)
                        (8 10 5) (8 11 6)
                        (9 12 4)
                        (10 12 2)
                        (11 12 5)))

(defun find-edges (vertex graph)
  (let ((edges nil))
    (dolist (e graph)
      (when (eql (car e) vertex)
        (push e edges)))
    edges))

;; find a next to vertex which gets a min cost to destination vertex
(defun find-vertex (vertex graph cost)
  (let ((edges (find-edges vertex graph))
        (node nil)
        (minval nil))
    (when edges
      (dolist (e edges)
        (cond ((null minval)
               (setf minval (+ (aref cost (second e)) (third e)))
               (setf node (second e)))
              (t (let ((val (+ (aref cost (second e)) (third e))))
                   (when (< val minval)
                     (setf minval val)
                     (setf node (second e))))))))
    (values minval node)))
      ;; (apply #'min (mapcar #'(lambda (edge)
      ;;                          (+ (third edge) (aref cost (second edge)))))))))

;; graph: list of edges
;; n: number of vertice
;; k: number of stages
(defun fgraph (graph k n)
  (let ((cost (make-array (1+ n) :initial-element 0))
        (d    (make-array n))          ;record the stratgy
        (p    (make-array (1+ k))))    ;store vertice on min cost path
    (loop for j from (1- n) downto 1
       do (multiple-value-bind (costj r) (find-vertex j graph cost)
            (when r
              (setf (aref cost j) costj
                    (aref d j) r))))
    (setf (aref p 1) 1
          (aref p k) n)
    (loop for j from 2 to (1- k)
       do (setf (aref p j) (aref d (aref p (1- j)))))
    p))

;; 向后处理算法多段图用逆邻接表来存储
;; should use reversed graph : (destination source power)
;; (mapcar #'(lambda (lst) (list (second lst) (first lst) (third lst))) *graph*)
(defun bgraph (graph k n)
  (let ((cost (make-array (1+ n) :initial-element 0))
        (d    (make-array (1+ n)))          ;record the stratgy
        (p    (make-array (1+ k))))    ;store vertice on min cost path
    (loop for j from 2 to n
       do (multiple-value-bind (costj r) (find-vertex j graph cost)
            (when r
              (setf (aref cost j) costj
                    (aref d j) r))))
    (setf (aref p 1) 1
          (aref p k) n)
    (loop for j from (1- k) downto 2
       do (setf (aref p j) (aref d (aref p (1+ j)))))
    p))
