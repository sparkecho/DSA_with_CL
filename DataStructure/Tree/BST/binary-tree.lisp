
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
