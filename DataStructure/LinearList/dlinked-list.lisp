;; Doubly Linked List
;; Reference: ANSI Common Lisp (Paul Graham)

(defstruct (dl (:print-function print-dl))
  prev data next)

(defun dl->list (lst)
  (if (dl-p lst)
	  (cons (dl-data lst) (dl->list (dl-next lst)))
	  lst))

(defun dl-insert (x lst)
  (let ((elt (make-dl :data x :next lst)))
	(when (dl-p lst)
	  (if (dl-prev lst)
		  (setf (dl-next (dl-prev lst)) elt
				(dl-prev elt) (dl-prev lst)))
	  (setf (dl-prev lst) elt))
	elt))

(defun dl-list (&rest args)
  (reduce #'dl-insert args :from-end t :initial-value nil))

(defun dl-remove (lst)
  (if (dl-prev lst)
	  (setf (dl-next (dl-prev lst)) (dl-next lst)))
  (if (dl-next lst)
	  (setf (dl-prev (dl-next lst)) (dl-prev lst)))
  (dl-next lst))
