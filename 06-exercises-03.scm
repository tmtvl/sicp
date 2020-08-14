(define (ackermann x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else
	 (ackermann (1- x)
		    (ackermann x (1- y))))))
