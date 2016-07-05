;; Bubble Sort

(defun bubble-sort (arr)
  (let ((flag nil)
		(end (1- (length arr))))
	(do ()
		(flag arr)
	  (setf flag t)
	  (do ((i 1 (+ i 1))
		   (j 0 (+ j 1)))
		  ((> i end))
		(if (< (aref arr i)
			   (aref arr j))
			(progn
			  (rotatef (aref arr i) (aref arr j))
			  (setf flag nil)))))))
