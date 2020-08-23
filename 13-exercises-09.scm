;; Exercise 2.17
(define (last-pair l)
  (cond ((null? l) '())
	((null? (cdr l)) l)
	(else (last-pair (cdr l)))))

;; Exercise 2.18
(define (my-reverse l)
  (define (R a b)
    (if (null? a)
	b
	(R (cdr a)
	   (cons (car a) b))))
  (R l '()))

;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (last-denomination? coin-values)
  (null? (cdr coin-values)))

(define (cc amount coin-values)
  (cond ((or (< amount 0)
	     (no-more? coin-values)) 0)
	((or (= amount 0)
	     (last-denomination? coin-values)) 1)
	(else
	 (+ (cc amount
		(except-first-denomination
		 coin-values))
	    (cc (- amount
		   (first-denomination
		    coin-values))
		coin-values)))))

;; Exercise 2.20
(define (same-parity a . l)
  (define (SP m)
    (cond ((null? m) '())
	  ((or (and (odd? a)
		    (even? (car m)))
	       (and (even? a)
		    (odd? (car m))))
	   (SP (cdr m)))
	  (else (cons (car m)
		      (SP (cdr m))))))
  (cons a (SP l)))

;; Exercise 2.21
(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.23
(define (my-for-each f l)
  (if (not (null? l))
      (begin
	(f (car l))
	(my-for-each f
		     (cdr l)))))

;; Exercise 2.27
(define (deep-reverse lol)
  (define (DR l result)
    (cond ((null? l) result)
	  ((not (pair? l))
	   (cons l result))
	  ((list? (car l))
	   (DR (cdr l)
	       (cons (DR (car l) '())
		     result)))
	  ((pair? (car l))
	   (DR (cdr l)
	       (cons (DR (cdar l) (caar l))
		     result)))
	  (else (DR (cdr l)
		    (cons (car l)
			  result)))))
  (DR lol '()))

;; Exercise 2.28
(define (fringe tree)
  (define (F t result)
    (cond ((null? t) result)
	  ((list? (car t))
	   (F (cdr t)
	      (append result
		      (F (car t) '()))))
	  (else (F (cdr t)
		   (append result (list (car t)))))))
  (F tree '()))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (let ((lbs (branch-structure (left-branch mobile)))
	(rbs (branch-structure (right-branch mobile))))
    (+ (if (pair? lbs)
	   (total-weight lbs)
	   lbs)
       (if (pair? rbs)
	   (total-weight rbs)
	   rbs))))

(define (balanced? mobile)
  (let ((lbs (branch-structure (left-branch mobile)))
	(lbl (branch-length (left-branch mobile)))
	(rbs (branch-structure (right-branch mobile)))
	(rbl (branch-length (right-branch mobile))))
    (and (if (pair? lbs) (balanced? lbs)
	     #t)
	 (if (pair? rbs) (balanced? rbs)
	     #t)
	 (= (* lbl
	       (if (pair? lbs)
		   (total-weight lbs)
		   lbs))
	    (* rbl
	       (if (pair? rbs)
		   (total-weight rbs)
		   rbs))))))
