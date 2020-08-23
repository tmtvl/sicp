(define (average x y)
  (/ (+ x y) 2.0))

(define (search f neg-point pos-point)
  (let ((midpoint (/ (+ neg-point pos-point) 2.0)))
    (if (< (abs (- neg-point pos-point)) 0.001)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((negative? test-value)
		 (search f midpoint pos-point))
		((positive? test-value)
		 (search f neg-point midpoint))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value)
		(positive? b-value))
	   (search f a b))
	  ((and (positive? a-value)
		(negative? b-value))
	   (search f b a))
	  (else (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
	  next
	  (try next))))
  (try first-guess))

(define (fsqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

;; Exercise 1.35
(define (fgr)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

;; Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (own-power-one-thousand)
  (fixed-point (lambda (x) (/ (log 1000)
			      (log x)))
	       2.0))

(define (own-power-one-thousand-dampened)
  (fixed-point (lambda (x) (average x
				    (/ (log 1000)
				       (log x))))
	       2.0))

;; Exercise 1.37
(define (cont-frac nf df k)
  (define (CF d i)
    (if (< i 1)
	d
	(CF (/ (nf i)
	       (+ (df i) d))
	    (1- i))))
  (CF 0 k))

(define (one-over-gr k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     k))

(define (cont-frac-recur nf df k)
  (define (CF i)
    (if (> i k)
	0
	(/ (nf i)
	   (+ (df i)
	      (CF (1+ i))))))
  (CF 1.0))

;; Exercise 1.38
(define (euler k)
  (define (df i)
    (if (= (euclidean-remainder i 3) 2)
	(* (1+ (euclidean-quotient i 3)) 2.0)
	i))
  (cont-frac (lambda (i) 1.0)
	     df k))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
	       (if (> i 1) (* x x) x))
	     (lambda (i)
	       (1- (* i 2)))
	     k))
