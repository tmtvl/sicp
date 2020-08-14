;; Exercise 1.11
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (1- n))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (letrec ((F (lambda (m x y z)
		(if (= m n)
		    z
		    (F
		     (1+ m)
		     y z
		     (+ z
			(* 2 y)
			(* 3 x)))))))
    (if (< n 3)
	n
	(F 2 0 1 2))))

;; Exercise 1.12
(define (pascals-row previous-row previous-element)
  (if (null? previous-row)
      (cons previous-element '())
      (cons (+ (previous-element (car previous-row)))
	    (pascals-row (cdr previous-row) (car previous-row)))))

(define (pascals-triangle n)
  (if (< n 2)
      '((1))
      (let ((t (pascals-triangle (1- n))))
	(cons (pascals-row (car t))
	      t))))

;; Exercise 1.13
(define (fib n)
  (letrec ((F (lambda (m a b)
		(if (= m n)
		    b
		    (F (1+ m)
		       b
		       (+ a b))))))
    (if (< n 1)
	n
	(F 1 0 1))))

(define (closest-to-golden-expt n)
  (/ (- (expt (/ (1+ (sqrt 5)) 2.0) n)
	(expt (/ (1- (sqrt 5)) 2.0) n))
     (sqrt 5)))

;; Exercise 1.16
(define (expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (expt-iter
		    a
		    (* b b)
		    (/ n 2)))
	(else (expt-iter
	       (* a b)
	       b
	       (1- n)))))

(define (it-expt x n)
  (expt-iter 1 x n))
