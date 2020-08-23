(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29
(define (simpsons-rule f a b n)
  (let ((h (/ (- b a) n)))
    (* (/ h 3)
     (sum (lambda (i)
	    (* (cond ((or (= i 0)
			  (= i n)) 1)
		     ((even? i) 2)
		     (else 4))
	       (f (+ a (* i h)))))
	  0 1+ n))))

;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 1+ n))

(define (approx-pi n)
  (define (term i)
    (if (even? i)
	(/ i (+ i 1.0))
	(/ (+ i 1.0) i)))
  (* (product term 2 1+ n) 4))

(define (recur-product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accu-sum term a next b)
  (accumulate + 0 term a next b))

(define (accu-product term a next b)
  (accumulate * 1 term a next b))

(define (it-accumulate combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x)
	      (combiner (term x) result))))
  (iter a null-value))

;; Exercise 1.33
(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x)
	      (if (filter x)
		  (combiner (term x) result)
		  result))))
  (iter a null-value))

(define (prime? x)
  (define (divisors from)
    (cond ((> (* from from) x) x)
	  ((= (euclidean-remainder x from) 0) from)
	  (else (divisors (+ from 2)))))
  (cond ((= x 2) #t)
	((= (euclidean-remainder x 2) 0) #f)
	(else (= (divisors 3) x))))

(define (sum-of-square-primes a b)
  (filtered-accumulate + 0 prime? square a 1+ b))

(define (product-of-relative-primes n)
  (define (relative-prime? x) (= (gcd x n) 1))
  (filtered-accumulate * 1 relative-prime? identity 1 1+ (1- n)))
