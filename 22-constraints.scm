(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1)
		(has-value? a2))
	   (set-value! sum
		       (+ (get-value a1)
			  (get-value a2))
		       dispatch))
	  ((and (has-value? a1)
		(has-value? sum))
	   (set-value! a2
		       (- (get-value sum)
			  (get-value a1))
		       dispatch))
	  ((and (has-value? a2)
		(has-value? sum))
	   (set-value! a1
		       (- (get-value sum)
			  (get-value a2))
		       dispatch))))
  (define (process-forget-value)
    (forget-value! sum dispatch)
    (forget-value! a1 dispatch)
    (forget-value! a2 dispatch)
    (process-new-value))
  (define (dispatch request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: ADDER" request))))
  (connect a1 dispatch)
  (connect a2 dispatch)
  (connect sum dispatch)
  dispatch)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1)
		    (= (get-value m1) 0))
	       (and (has-value? m2)
		    (= (get-value m2) 0)))
	   (set-value! product 0 dispatch))
	  ((and (has-value? m1)
		(has-value? m2))
	   (set-value! product
		       (* (get-value m1)
			  (get-value m2))
		       dispatch))
	  ((and (has-value? m1)
		(has-value? product))
	   (set-value! m2
		       (/ (get-value product)
			  (get-value m1))
		       dispatch))
	  ((and (has-value? m2)
		(has-value? product))
	   (set-value! m1
		       (/ (get-value product)
			  (get-value m2))
		       dispatch))))
  (define (process-forget-value)
    (forget-value! product dispatch)
    (forget-value! m1 dispatch)
    (forget-value! m2 dispatch)
    (process-new-value))
  (define (dispatch request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 dispatch)
  (connect m2 dispatch)
  (connect product dispatch)
  dispatch)

(define (constant value connector)
  (define (dispatch request)
    (error "Unknown request: CONSTANT" request))
  (connect connector dispatch)
  (set-value! connector value dispatch)
  dispatch)

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (dispatch request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: PROBE" request))))
  (connect connector dispatch)
  dispatch)

(define (make-connector)
  (let ((value #f)
	(informant #f)
	(constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? dispatch))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant #f)
		 (for-each-except retractor
				  inform-about-no-value
				  constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints
	    (cons new-constraint constraints)))
      (if (has-value? dispatch)
	  (inform-about-value new-constraint))
      'done)
    (define (dispatch request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation: CONNECTOR" request))))
    dispatch))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector value informant)
  ((connector 'set-value!) value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; Exercise 3.33
(define (averager a b c)
  (let ((s (make-connector))
	(t (make-connector)))
    (constant 2 t)
    (adder a b s)
    (multiplier c t s)
    'ok))

;; Exercise 3.55
(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
	   (if (< (get-value b) 0)
	       (error "Square less than 0: SQUARER" (get-value b))
	       (set-value! a
			   (sqrt (get-value b))
			   dispatch)))
	  ((has-value? a)
	   (set-value! b
		       (* (get-value a)
			  (get-value a))
		       dispatch))))
  (define (process-forget-value)
    (forget-value! b dispatch)
    (forget-value! a dispatch)
    (process-new-value))
  (define (dispatch request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: SQUARER" request))))
  (connect a dispatch)
  (connect b dispatch)
  dispatch)

;; Exercise 3.57
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((y (make-connector)))
    (constant x y)
    y))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9)
	      (cv 5))
	  x)
      (cv 32)))
