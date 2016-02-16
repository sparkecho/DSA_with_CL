
;; A set of function to manipulate a binary tree
;;       Dependence: 
;;             bst.lisp
;;             queue.lisp								



;; Level Traverse of Binary Tree

(defun traverse (bt)
  (let ((acc nil)
		(q (make-queue)))
	(enqueue bt q)

	(do ((e (dequeue q) (dequeue q)))
		(nil)
	  (push (node-elt e) acc)
	  (cond ((leafp e) (if (qempty q) (return (nreverse acc))))
			((null (node-l e)) (enqueue (node-r e) q))
			((null (node-r e)) (enqueue (node-l e) q))
			(t (enqueue (node-l e) q)
			   (enqueue (node-r e) q))))))




(defun leafp (tr)
  (if (null tr)
	  nil
	  (and (null (node-l tr))
		   (null (node-r tr)))))




;; 判断给定二叉树是否为完全二叉树
(defun full-bt-p (bt)
  (let ((q (make-queue)))
	(enqueue bt q)
	(do ((e (dequeue q) (dequeue q)))
		(nil)
	  (cond ((null e) (if (qempty q)
						  (return t)
						  (return nil)))

			((leafp e) (if (qempty q)
						   (return t)))

			((and (null (node-l e)) (node-r e)) ;左子树为空, 右子树不为空的一定不是完全二叉树
			 (return nil))

			(t (enqueue (node-l e) q)
			   (enqueue (node-r e) q))))))




;; 判断两个二叉树是否相似(即, 具有相同的结构)
(defun similar (bt1 bt2)
  (cond ((null bt1) (if (null bt2) t))
		((leafp bt1) (if (leafp bt2) t))
		(t (if (or (null bt2) (leafp bt2))
			   nil
			   (and (similar (node-l bt1) (node-l bt2))
					(similar (node-r bt1) (node-r bt2)))))))




;; 返回一棵二叉树中节点的个数
(defun count-node (bt)
  (if (null bt)
	  0
	  (+ 1
		 (count-node (node-l bt))
		 (count-node (node-r bt)))))


;; 返回一棵二叉树中边的个数
(defun count-edge (bt)
  (1- (count-node bt)))



;; 打印一棵二叉树中最长的一条路径
(defun print-path (bt)
  (format t "~{~A ~}" (longest-path bt)))


;; 返回由给定二叉树中最长路径中各个节点元素构成的列表
(defun longest-path (bt)
  (if (null bt)
	  nil
	  (cons (node-elt bt)
			(longer-list (longest-path (node-l bt))
						 (longest-path (node-r bt))))))



;; 返回给定的两个列表中较长的列表, 若等长则返回第一个
(defun longer-list (lst1 lst2)
  (labels ((longer (lst1 lst2)
			 (cond ((null lst2) 1)
				   ((null lst1) 2)
				   (t (longer (cdr lst1) (cdr lst2))))))
	(if (= (longer lst1 lst2) 1)
		lst1
		lst2)))
