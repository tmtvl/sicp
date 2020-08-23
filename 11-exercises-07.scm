(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.0001))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (sqrte x)
  (fixed-point (average-damp (lambda (y)
			       (/ x y)))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y)
			       (/ x (square y))))
	       1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x 0.0001))
	  (g x))
       0.0001)))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
	  ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-sqrt x)
  (newtons-method (lambda (y)
		    (- (square y) x))
		  1.0))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (- (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (define (R x i)
    (if (> i n)
	x
	(R (f x)
	   (1+ i))))
  (lambda (x)
    (R x 1)))

;; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x 0.0001))
	  (f x)
	  (f (+ x 0.0001)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;; Exercise 1.45
(define (limited-fixed-point f first-guess max-steps)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.0001))
  (define (try guess step)
    (let ((next (f guess)))
      (cond ((close-enough? next guess) next)
	    ((> step max-steps) #f)
	    (else
	     (try next (1+ step))))))
  (try first-guess 1))

(define (display-dampening-nth-root x n)
  (define (try steps)
    (let ((test-value (limited-fixed-point
		       ((repeated average-damp steps)
			(lambda (y)
			  (/ x (expt y (1- n)))))
		       1.0
		       100000)))
      (display steps)
      (newline)
      (if test-value
	  test-value
	  (try (1+ steps)))))
  (try 1))

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  try)

(define (iter-sqrt x)
  ((iterative-improve
    (lambda (y)
      (< (abs (- y (/ x y))) 0.0001))
    (average-damp
     (lambda (y) (/ x y))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (x)
      (< (abs (- x (f x))) 0.0001))
    f)
   first-guess))
