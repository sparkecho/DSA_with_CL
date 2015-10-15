;; Ackerman

;; Recursion

(defun ackerman (m n)
  (cond ((zerop m) (1+ n))
		((zerop n) (ackerman (1- m) 1))
		(t (ackerman (1- m)
					 (ackerman m (1- n))))))							   


;; Iteration

(defun ack (m n)
  (let ((stack nil)
		(cnt -1))
	(push (cons m n) stack)

	(do ((top (pop stack) (pop stack)))
		(nil)
	  (let ((m* (car top)) (n* (cdr top)))
		(cond ((< n* 0) (push (cons m* cnt) stack))
			  ((zerop m*)
			   (setf cnt (1+ n*))
			   (if (null stack) (return cnt)))
			  ((zerop n*) (push (cons (1- m*) 1) stack))
			  (t (push (cons (1- m*) -1) stack)
				 (push (cons m* (1- n*)) stack)))))))
			   
