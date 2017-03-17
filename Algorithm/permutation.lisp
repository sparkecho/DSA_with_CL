;; 生成全排列
;; 根据给定的数组生成包含该数组的所有全排列的列表
;; Generate permutation
;; Generate a list that includes all the permutations of the given vector
(defun permutation (vec)
  (let ((result nil))
    (labels ((perm (vec k len)
               (if (= k len)
                   (push (copy-seq vec) result)
                   (loop for i from k below len
                      do (progn (rotatef (aref vec i) (aref vec k))
                                (perm vec (1+ k) len)
                                (rotatef (aref vec k) (aref vec i)))))))
      (perm vec 0 (length vec)))
    result))


;; 使用列表的不同版本
;; Different version of using list
(defun permutation-list (lst)
  (let ((result nil))
    (labels ((perm (lst k len)
               (if (= k len)
                   (push (copy-seq lst) result)
                   (loop for i from k below len
                      do (progn (rotatef (elt lst i) (elt lst k))
                                (perm lst (1+ k) len)
                                (rotatef (elt lst k) (elt lst i)))))))
      (perm lst 0 (length lst)))
    result))
