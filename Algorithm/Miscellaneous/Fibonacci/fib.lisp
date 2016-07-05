;; SICP Chapter 1
;; Text 1.19

(defun fib (n)
  (fib-iter 1 0 0 1 n))

;; init: (1, 0)
;; a <--  a + b
;; b <--  a
;; 变换规则 Tpq
;; a <--  bq + aq + ap
;; b <--  bp + aq
(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
		((evenp count)
		 (fib-iter a
				   b
				   (* 2 (square q))		  ;compute p'
				   (+ (* p q) (square q)) ;compute q'
				   (/ count 2)))
		(t (fib-iter (+ (* b q) (* a q) (* a p))
					 (+ (* b q) (* a q))
					 p
					 q
					 (- count 1)))))

(defun square (x)
  (* x x))


(defun simp-fib1 (n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(t (+ (simp-fib1 (- n 1))
			  (simp-fib1 (- n 2))))))


(defun simp-fib2 (n)
  (simp-fib-iter 1 0 n))

(defun simp-fib-iter (a b count)
  (if (= count 0)
	  b
	  (simp-fib-iter (+ a b) a (- count 1))))
