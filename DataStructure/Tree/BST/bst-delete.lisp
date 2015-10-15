;;; From ANSI Common Lisp

; If you have questions or comments about this code, or you want
; something I didn't include, send mail to lispcode@paulgraham.com.

; This code is copyright 1995 by Paul Graham, but anyone who wants
; to use it is free to do so.

(defun bst-delete (obj bst <)
  (if (null bst)
      nil       
      (if (eql obj (node-elt bst))
          (del-root bst)
          (progn
            (if (funcall < obj (node-elt bst))
                (setf (node-l bst) (bst-delete obj (node-l bst) <))
                (setf (node-r bst) (bst-delete obj (node-r bst) <)))
            bst)))) 
  
(defun del-root (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t        (if (zerop (random 2))
                        (cutnext r bst nil)
                        (cutprev l bst nil))))))

(defun cutnext (bst root prev)
  (if (node-l bst)
      (cutnext (node-l bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-l prev)   (node-r bst))
            root)
          (progn
            (setf (node-l bst)    (node-l root))
            bst))))

(defun cutprev (bst root prev)
  (if (node-r bst)
      (cutprev (node-r bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-r prev)   (node-l bst))
            root)
          (progn
            (setf (node-r bst)    (node-r root))
            bst))))

(defun replace-node (old new)
  (setf (node-elt old) (node-elt new)
        (node-l   old) (node-l   new)
        (node-r   old) (node-r   new)))

(defun cutmin (bst par dir)
  (if (node-l bst)
      (cutmin (node-l bst) bst :l)
      (progn
        (set-par par dir (node-r bst))
        (node-elt bst))))

(defun cutmax (bst par dir)
  (if (node-r bst)
      (cutmax (node-r bst) bst :r)
      (progn
        (set-par par dir (node-l bst))
        (node-elt bst))))

(defun set-par (par dir val)
  (case dir
    (:l (setf (node-l par) val))
    (:r (setf (node-r par) val))))
