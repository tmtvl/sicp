(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s)
		  (1- n))))

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
