(define (mmemq item l)
  (cond ((null? l) #f)
	((eq? item (car l)) l)
	(else (mmemq item (cdr l)))))

;; Exercise 2.54
(define (mequal? a b)
  (cond ((null? a) (null? b))
	((null? b) #f)
	((not (pair? a))
	 (and (not (pair? b))
	      (eq? a b)))
	(else (and (pair? b)
		   (mequal? (car a) (car b))
		   (mequal? (cdr a) (cdr b))))))


;; Symbolic Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unknown expression type: DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1)
	      (number? a2))
	 (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
	     (=number? m2 0))
	 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1)
	      (number? m2))
	 (* m1 m2))
	(else (list '* m1 m2))))

;; Exercise 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	; the derivation of u^n is n*(u^n-1)*du
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product
			(make-exponentiation (base exp)
					     (make-sum
					      (exponent exp)
					      -1))
			(deriv (base exp) var))))
	(else
	 (error "unknown expression type: DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((and (number? b)
	      (number? e))
	 (expt b e))
	(else (list '** b e))))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

;; Exercise 2.57
(define (augend s)
  (define (recur l)
    (if (null? l)
	'()
	(make-sum (car l)
		  (recur (cdr l)))))
  (recur (cddr s)))

(define (multiplicand p)
  (define (recur l)
    (if (null? l)
	'()
	(make-product (car l)
		      (recur (cdr l)))))
  (recur (cddr p)))

(define (make-sum a1 a2)
  (cond ((null? a2) a1)
	((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1)
	      (number? a2))
	 (+ a1 a2))
	((sum? a2)
	 (if (and (number? a1)
		  (number? (addend a2)))
	     (make-sum (+ a1 (addend a2))
		       (augend a2))
	     (append (list '+ a1)
		     (cdr a2))))
	((number? a2)
	 (list '+ a2 a1))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((null? m2) m1)
	((or (=number? m1 0)
	     (=number? m2 0))
	 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1)
	      (number? m2))
	 (* m1 m2))
	((product? m2)
	 (if (and (number? m1)
		  (number? (multiplier m2)))
	     (make-product (* m1 (multiplier m2))
			   (multiplicand m2))
	     (append (list '* m1)
		     (cdr m2))))
	((number? m2)
	 (list '* m2 m1))
	(else (list '* m1 m2))))

;; Exercise 2.58
(define (sum? x)
  (eq? (cadr x) '+))

(define (product? x)
  (eq? (cadr x) '*))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1)
	      (number? a2))
	 (+ a1 a2))
	(else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
	     (=number? m2 0))
	 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1)
	      (number? m2))
	 (* m1 m2))
	(else (list m1 '* m2))))
