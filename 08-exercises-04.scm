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

;; Exercise 1.17
(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
	((= b 1) a)
	((even? b) (fast-mult (double a)
			      (halve b)))
	(else (+ a (fast-mult a (1- b))))))

;; Exercise 1.18
(define (mult-iter a b n)
  (cond ((= n 0) a)
	((even? n) (mult-iter
		    a
		    (double b)
		    (halve n)))
	(else (mult-iter
	       (+ a b)
	       b
	       (1- n)))))

(define (it-mult a b)
  (mult-iter 0 a b))

;; Exercise 1.19
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((and #t (even? count))
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))
		   (+ (* p q) (* q q) (* q p))
		   (halve count)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(1- count)))))

(define (it-fib n)
  (fib-iter 1 0 0 1 n))

;; Exercise 1.21
(define (divides? a b)
  (= (euclidean-remainder a b) 0))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else (find-divisor n (1+ test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

;; Exercise 1.22
(define (prime? n)
  (= (smallest-divisor n) n))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (get-internal-real-time) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (get-internal-real-time)))

(define (find-three-primes-greater-than n)
  (letrec ((PT (lambda (candidate needed)
		 (cond ((= needed 0)
			(newline))
		       ((prime? candidate)
			(timed-prime-test candidate)
			(PT (1+ candidate) (1- needed)))
		       (else (PT (1+ candidate) needed))))))
    (PT n 3)))

;; Exercise 1.23
(define (next-divisor n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else (find-divisor n (next-divisor test-divisor)))))
