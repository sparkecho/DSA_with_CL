;; Binary Search Tree

;; 二叉搜索树构建示例:
;; > (defparameter bst nil)
;; NIL
;; > (dolist (e '(5 8 4 2 9 6 7))
;;     (setf bst (bst-insert e bst #'<)))
;; NIL


(defstruct (node (:print-function
				  (lambda (n s d)
					(declare (ignore d))
					(format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))



;; 向二叉搜索树中以 指定顺序　插入一个对象
(defun bst-insert (obj bst <)
  (if (null bst)
	  (make-node :elt obj)
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(make-node
				 :elt elt
				 :l (bst-insert obj (node-l bst) <)
				 :r (node-r bst))
				(make-node
				 :elt elt
				 :r (bst-insert obj (node-r bst) <)
				 :l (node-l bst)))))))


;; 查找一个对象是否是指定二叉搜索树的一个元素, 如果是则返回以元素为根的子二叉搜索树
(defun bst-find (obj bst <)
  (if (null bst)
	  nil
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(bst-find obj (node-l bst) <)
				(bst-find obj (node-r bst) <))))))



;; 从指定二叉树中删除指定对象所在节点
(defun bst-remove (obj bst <)
  (if (null bst)
	  nil
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			(percolate bst)
			(if (funcall < obj elt)
				(make-node :elt elt
						   :l (bst-remove obj (node-l bst) <)
						   :r (node-r bst))

				(make-node :elt elt
						   :l (node-l bst)
						   :r (bst-remove obj (node-r bst) <)))))))


;;
(defun percolate (bst)
  (cond ((null (node-l bst)) (if (null (node-r bst))
								 nil
								 (rperc bst)))
		((null (node-r bst)) (lperc bst))
		(t (if (zerop (random 2))
			   (lperc bst)
			   (rperc bst)))))

;;
(defun lperc (bst)
  (make-node :elt (node-elt (node-r bst))
			 :l (percolate (node-l bst))
			 :r (node-r bst)))

;;
(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
			 :l (node-l bst)
			 :r (percolate (node-r bst))))
				



;; 查找二叉搜索树中最小的元素
(defun bst-min (bst)
  (and bst
	   (or (bst-min (node-l bst)) bst)))


;; 查找二叉搜索树中最大的元素
(defun bst-max (bst)
  (and bst
	   (or (bst-max (node-r bst)) bst)))





;; 二叉树的中序遍历(中根遍历), 对每个元素执行 fn 操作
(defun in-order (fn bt)
  (when bt
    (in-order fn (node-l bt))
    (funcall  fn (node-elt bt))
    (in-order fn (node-r bt))))



;; 二叉树的先序遍历(先根遍历)
(defun pre-order (fn bt)
  (when bt
	(funcall fn (node-elt bt))
    (pre-order fn (node-l bt))
    (pre-order fn (node-r bt))))



;; 二叉树的后序遍历(后根遍历)
(defun post-order (fn bt)
  (when bt
    (post-order fn (node-l bt))
    (post-order fn (node-r bt))
	(funcall fn (node-elt bt))))



;; 
(defun pre-order-vec (bt vec)
  (if (null bt)
	  vec
	  (progn (vector-push (node-elt bt) vec)
			 (pre-order-vec (node-l bt) vec)
			 (pre-order-vec (node-r bt) vec)
			 vec)))
