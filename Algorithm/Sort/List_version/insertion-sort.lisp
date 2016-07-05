;; Insertion Sort on List

(defun insertion-sort (lst &optional (filter #'identity))
  (cond ((null lst) nil)
		(t (insert-one (car lst)
					   (insertion-sort (cdr lst))
					   filter))))

(defun insert-one (item lst &optional (filter #'identity))
  (cond ((null lst) (list item))
		((< (funcall filter item)
			(funcall filter (car lst)))
		 (cons item lst))
		(t (cons (car lst)
				 (insert-one item (cdr lst))))))
