(define (stream-ref s n)
  (cond ((stream-null? s) the-empty-stream)
	((= n 0) (stream-car s))
	(else (stream-ref (stream-cdr s)
			  (1- n)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc
			       (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc
			      (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (1+ low) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (my-force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? #f)
	(result #f))
    (lambda ()
      (if (not already-run?)
	  (begin
	    (set! result (proc))
	    result)
	  result))))

(define-syntax my-delay
  (syntax-rules ()
    ((my-delay proc)
     (memo-proc (lambda () proc)))))

(define the-empty-stream '())

(define (stream-null? s)
  (null? s))

(define (prime? x)
  (define (iter a)
    (cond ((> (* a a) x) #t)
	  ((= (euclidean-remainder x a) 0) #f)
	  (else (iter (+ a 2)))))
  (cond ((= x 2) #t)
	((or (< x 2)
	     (= (euclidean-remainder x 2) 0))
	 #f)
	(else (iter 3))))

;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc
		    (map stream-cdr argstreams))))))


;; Infinite streams
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (euclidean-remainder x y) 0))

(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7)))
				 integers))

(define (lucas-sequence a b)
  (cons-stream a (lucas-sequence b (+ a b))))

(define fibs (lucas-sequence 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes
  (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
   0
   (cons-stream
    1
    (add-streams (stream-cdr fibs)
		 fibs))))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x)
     (* x factor))
   stream))

(define double
  (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (square x)
  (* x x))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
	  ((divisible? n (stream-car ps)) #f)
	  (else (iter (stream-cdr ps)))))
  (iter primes))

(define (stream->list s n)
  (if (or (stream-null? s)
	  (= n 0))
      '()
      (cons (stream-car s)
	    (stream->list (stream-cdr s)
			  (1- n)))))

;; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
	       (mul-streams factorials
			    (stream-cdr integers))))

;; Exercise 3.55
(define (partial-sums s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (stream-car s)
       (partial-sums
	(if (stream-null? (stream-cdr s))
	    (stream-cdr s)
	    (cons-stream
	     (+ (stream-car s)
		(stream-car (stream-cdr s)))
	     (stream-cdr (stream-cdr s))))))))

;; Exercise 3.56
(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((a (stream-car s1))
	       (b (stream-car s2)))
	   (cond ((< a b)
		  (cons-stream
		   a
		   (stream-merge (stream-cdr s1) s2)))
		 ((> a b)
		  (cons-stream
		   b
		   (stream-merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream
		   a
		   (stream-merge (stream-cdr s1)
				 (stream-cdr s2)))))))))

(define hamming
  (cons-stream
   1
   (stream-merge
    (scale-stream hamming 2)
    (stream-merge
     (scale-stream hamming 3)
     (scale-stream hamming 5)))))

;; Exercise 3.59
(define (integrate-series s)
  (stream-map * s
	      (stream-map (lambda (x) (/ 1 x))
			  integers)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (mul-series (stream-cdr s1) s2)
			    (scale-stream (stream-cdr s2)
					  (stream-car s1)))))

;; Exercise 3.61
(define (invert-unit-series s)
  (cons-stream
   1
   (mul-series (scale-stream (stream-cdr s) -1)
	       (invert-unit-series s))))

;; Exercise 3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 1)
      (mul-series s1 (invert-unit-series s2))
      (error "Constant term of denominator is not 1: DIV-SERIES" s2)))

(define tangent-series (div-series sine-series cosine-series))


;; Exploiting the Stream paradigm
(define (sqrt-improve guess x)
  (/ (+ guess
	(/ x guess))
     2.0))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
		 guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;; Exercise 3.64
(define (stream-limit stream tolerance)
  (define (SL s p)
    (cond ((stream-null? s) the-empty-stream)
	  ((< (abs (- (stream-car s)
		      p))
	      tolerance)
	   (stream-car s))
	  (else (SL (stream-cdr s) (stream-car s)))))
  (if (stream-null? stream)
      stream
      (SL (stream-cdr stream) (stream-car stream))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; Exercise 3.65
(define flipflop
  (cons-stream 1.0
	       (scale-stream flipflop -1)))

(define ln2-series
  (partial-sums (stream-map / flipflop integers)))

(define ln2-series-two
  (euler-transform ln2-series))

(define ln2-series-three
  (accelerated-sequence euler-transform ln2-series))
