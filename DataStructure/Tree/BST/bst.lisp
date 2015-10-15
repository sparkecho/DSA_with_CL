;; Binary Search Tree


(defstruct (node (:print-function
				  (lambda (n s d)
					(declare (ignore d))
					(format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))



(defun bst-insert (obj bst <)
  (if (null bst)
	  (make-node :elt obj)
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(make-node
				 :elt elt
				 :l (bst-insert obj (node-l bst) <)
				 :r (node-r bst))
				(make-node
				 :elt elt
				 :r (bst-insert obj (node-r bst) <)
				 :l (node-l bst)))))))



(defun bst-find (obj bst <)
  (if (null bst)
	  nil
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(bst-find obj (node-l bst) <)
				(bst-find obj (node-r bst) <))))))



(defun bst-min (bst)
  (and bst
	   (or (bst-min (node-l bst)) bst)))


(defun bst-max (bst)
  (and bst
	   (or (bst-max (node-r bst)) bst)))



(defun in-order (bst)
  (if (null bst)
	  nil
	  (progn (in-order (node-l bst))
			 (format t "~A " (node-elt bst))
			 (in-order (node-r bst)))))


(defun pre-order (bst)
  (if (null bst)
	  nil
	  (progn (format t "~A " (node-elt bst))
			 (pre-order (node-l bst))			 
			 (pre-order (node-r bst)))))


(defun post-order (bst)
  (if (null bst)
	  nil
	  (progn (post-order (node-l bst))
			 (post-order (node-r bst))
			 (format t "~A " (node-elt bst)))))



(defun pre-order-vec (bst vec)
  (if (null bst)
	  vec
	  (progn (vector-push (node-elt bst) vec)
			 (pre-order-vec (node-l bst) vec)
			 (pre-order-vec (node-r bst) vec)
			 vec)))
