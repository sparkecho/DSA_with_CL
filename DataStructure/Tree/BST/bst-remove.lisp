;;; From ANSI Common Lisp

; If you have questions or comments about this code, or you want
; something I didn't include, send mail to lispcode@paulgraham.com.

; This code is copyright 1995 by Paul Graham, but anyone who wants
; to use it is free to do so.

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node
                  :elt elt
                  :l (bst-remove obj (node-l bst) <)
                  :r (node-r bst))
                (make-node
                  :elt elt
                  :r (bst-remove obj (node-r bst) <)
                  :l (node-l bst)))))))

(defun percolate (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2))
                 (make-node :elt (node-elt (bst-max l))
                            :r r
                            :l (bst-remove-max l))
                 (make-node :elt (node-elt (bst-min r))
                            :r (bst-remove-min r)
                            :l l)))))) 

(defun bst-remove-min (bst)
  (if (null (node-l bst))  
      (node-r bst)
      (make-node :elt (node-elt bst)
                 :l   (bst-remove-min (node-l bst))
                 :r   (node-r bst))))

(defun bst-remove-max (bst)
  (if (null (node-r bst)) 
      (node-l bst)
      (make-node :elt (node-elt bst)
                 :l (node-l bst)
                 :r (bst-remove-max (node-r bst)))))