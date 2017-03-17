(defun place (k x)             ;x is an array
  (let ((result t))
    (loop for i from 1 below k
       do (if (or (= (aref x k) (aref x i))
                  (= (abs (- (aref x i) (aref x k))) (abs (- i k))))
              (return (setf result nil))))
    result))

;; x: stands for the array of cols that Queens are in
(defun nqueens (n)
  (let ((x (make-array (+ n 1) :initial-element 0)))
    (do ((k 1))
        ((<= k 0))
      (incf (aref x k))
      ;; (format t "outer: ~A, k = ~A~%" x k) ;for test
      (do ()
          ((or (> (aref x k) n) (place k x)))
        (incf (aref x k))
        ;; (format t "inner: ~A, k = ~A~%" x k) ;for test
        )
      (if (<= (aref x k) n)
          (if (= k n)
              (format t "~A~%" x)
              (progn (incf k) (setf (aref x k) 0)
                     ;; (format t "in if: ~A, k = ~A~%" x k) ;for test
              ))
          (decf k)))))


(defun node-num (n)
  (let ((acc 1))
    (1+ (loop for j from 0 below n
           sum (progn (setf acc 1)
                      (loop for i from 0 to j do (setf acc (* acc (- n i))))
                      acc)))))
