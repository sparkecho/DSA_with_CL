;; Queue

(defun make-queue ()
  (cons nil nil))

(defun enqueue (e q)
  (cond ((null (car q)) (setf (car q) (list e)
							  (cdr q) (car q)))
		(t (setf (cdr (cdr q)) (list e)
				 (cdr q) (cdr (cdr q))))))

(defun dequeue (q)
  (cond ((null (car q)) nil)
		(t (pop (car q)))))


(defun qempty (q)
  (null (car q)))
