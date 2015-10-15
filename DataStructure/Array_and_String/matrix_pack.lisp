;; To test the usage of package in Common Lisp.

(cl:in-package :cl-user)
(cl:defpackage :matrix
  (:use :cl)
  (:export :mprint
		   :mmake
		   :m+
		   :m-
		   :m*
		   :mtranspose))

(cl:in-package :matrix)

(defun mprint (mx)
  (let ((row (array-dimension mx 0))
		(col (array-dimension mx 1)))
	(dotimes (i row)
	  (dotimes (j col)
		(format t "~A~D" #\tab (aref mx i j)))
	  (format t "~%"))))

(defun mmake (row col)
  (let ((mx (make-array `(,row ,col) :element-type 'number)))
    (dotimes (i row)
      (dotimes (j col)
		(setf (aref mx i j) (read))))
    mx))



;; 矩阵加法
(defun m+ (mx1 mx2)
  (m+- mx1 mx2 '+))

;; 矩阵减法
(defun m- (mx1 mx2)
  (m+- mx1 mx2 '-))

;; 矩阵加减法
(defun m+- (mx1 mx2 sign)
  (let ((dims1 (array-dimensions mx1))
		(dims2 (array-dimensions mx2)))
    (and (equal dims1 dims2)		;dimensions should be same	 
		 (let ((mx3 (make-array dims1 :initial-element 'number))
			   (row (car dims1))
			   (col (second dims1)))
		   (dotimes (i row)
			 (dotimes (j col)
			   (setf (aref mx3 i j) (funcall sign
											 (aref mx1 i j)
											 (aref mx2 i j)))))
		   mx3))))



;; 矩阵乘法
(defun m* (mx1 mx2)
  (let ((row1 (array-dimension mx1 0))
		(col1 (array-dimension mx1 1))
		(row2 (array-dimension mx2 0))
		(col2 (array-dimension mx2 1)))
    (if (= col1 row2)
		(mx* mx1 mx2
			 (make-array `(,row1 ,col2) :initial-element 'number)
			 row1 col1 col2)
		(error "Cannot multiply these two -."))))


(defun mx* (mx1 mx2 mx3 row1 col1 col2)
  (dotimes (i row1 mx3)
    (dotimes (j col2)
      (let ((val 0))
		(dotimes (k col1)
		  (incf val (* (aref mx1 i k) (aref mx2 k j))))
		(setf (aref mx3 i j) val)))))




;; 矩阵转制
(defun mtranspose (mx)
  (let ((row (array-dimension mx 0))
		(col (array-dimension mx 1)))
	(let ((mxt (make-array `(,col ,row))))
	  (dotimes (i row)
		(dotimes (j col)
		  (setf (aref mxt j i) (aref mx i j))))
	  mxt)))

