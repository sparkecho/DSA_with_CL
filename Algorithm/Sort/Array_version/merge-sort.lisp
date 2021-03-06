;; Merge Sort on Array
(defun merge-sort (arr)
  (let ((len (length arr)))
    (cond ((zerop len) (vector))
          ((eql len 1) arr)
          ((eql len 2) (if (> (aref arr 0)
                              (aref arr 1))
                           (swap arr 0 1)))
          (t (let ((mid (floor (/ len 2))))
               (setf arr (merge 'vector
                                (merge-sort (subseq arr 0 mid))
                                (merge-sort (subseq arr mid len))
                                #'<)))))
    arr))

(defun swap (arr i j)
  (let ((temp (aref arr i)))
    (setf (aref arr i) (aref arr j)
          (aref arr j) temp)
    arr))
