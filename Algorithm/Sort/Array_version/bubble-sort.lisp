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


;; 升序
(defun bubble-sort1 (arr)
  (let ((len (length arr)))
    (loop for k from 0 below len
       do (loop for i from 1 below len
             do (let ((j (- i 1)))
                  (if (< (aref arr i)
                         (aref arr j))
                      (rotatef (aref arr i) (aref arr j))))))
    arr))
