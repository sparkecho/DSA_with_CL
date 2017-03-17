;; Quick Sort

(defun quick-sort (vec &optional (start 0) (end -1))
  (let ((len (1- (length vec))))
    (if (= end -1)
        (quicksort vec start len)
        (quicksort vec start end))))

(defun quicksort (vec l r)
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2))))
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
        (rotatef (svref vec i) (svref vec j))
        (incf i)
        (decf j)))
    (if (>= (- j l) 1) (quicksort vec l j))
    (if (>= (- r i) 1) (quicksort vec i r)))
  vec)
