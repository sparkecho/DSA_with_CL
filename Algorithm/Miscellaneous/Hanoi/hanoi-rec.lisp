
;; The algolrithm of Tower of Hanoi.
(defun hanoi (n x y z)
  (cond ((= n 1) (move x z))
		(t (hanoi (- n 1) x z y)
		   (move x z)
		   (hanoi (- n 1) y x z))))

;; Move one disk at once.
(defun move (a b)
  (format t "~A -> ~A~%" a b)
  'done)
