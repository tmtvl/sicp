;; Exercise 3.1
(define (make-accumulator init)
  (lambda (x)
    (set! init (+ init x))
    init))

;; Exercise 3.2
(define (make-monitored f)
  (let ((calls 0))
    (lambda (i)
      (cond ((eq? i 'how-many-calls?) calls)
	    ((eq? i 'reset-count) (set! calls 0))
	    (else (set! calls (1+ calls))
		  (f i))))))

;; Exercise 3.3
(define (make-account balance password)
  (define wrong-pass 0)
  (define (call-the-cops)
    (display "The police has been notified")
    (newline))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (not (eq? p password))
	(lambda (x)
	  (set! wrong-pass (1+ wrong-pass))
	  (if (>= wrong-pass 7)
	      (call-the-cops))
	  "Incorrect password")
	(begin
	  (set! wrong-pass 0)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request: MAKE-ACCOUNT" m))))))
  dispatch)


;; Benefits of introducing assignment
(define rand
  (let ((x (random 10000)))
    (lambda ()
      (set! x (random 10000))
      x)))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (1- trials-remaining)
		 (1+ trials-passed)))
	  (else
	   (iter (1- trials-remaining)
		 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; Exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (random-x)
    (random-in-range x1 x2))
  (define (random-y)
    (random-in-range y1 y2))
  (define (experiment)
    (P (random-x) (random-y)))
  (monte-carlo trials experiment))

;; Exercise 3.6
(define rand
  (let ((v 0))
    (lambda (op)
      (cond ((eq? op 'generate)
	     (set! v (1+ v))
	     v)
	    ((eq? op 'reset)
	     (lambda (x)
	       (set! v x)))))))

;; Exercise 3.7
(define (make-joint acct pass1 pass2)
  (lambda (p o)
    (if (eq? p pass2)
	(acct pass1 o)
	"Incorrect password")))

;; Exercise 3.8
(define f
  (let ((a 0)
	(b 0))
    (lambda (x)
      (set! a b)
      (set! b x)
      a)))
