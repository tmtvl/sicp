(define (my-filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (my-filter predicate (cdr sequence))))
	(else (my-filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
	    (enumerate-interval (1+ low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (list? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (square x)
  (* x x))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
  (define (F a b i)
    (if (= i n)
	a
	(F b (+ a b) (1+ i))))
  (F 0 1 0))

(define (even-fibs n)
  (filter even? (map fib (enumerate-interval 0 n))))

(define (list-fib-squares n)
  (map square (map fib (enumerate-interval 0 n))))

;; Exercise 2.33
(define (amap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (aappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (alength sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coefficient higher-terms)
		(+ this-coefficient (* higher-terms x)))
	      0
	      coefficient-sequence))

;; Exercise 2.35
(define (count-leaves t)
  (accumulate
   (lambda (elem count)
     (+ count
	(if (list? elem)
	    (count-leaves elem)
	    1)))
   0
   t))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;; Exercise 2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(= (fold-right + 0 (list 1 2 3))
   (fold-left + 0 (list 1 2 3)))

;; Exercise 2.39
(define (rreverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (lreverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime? x)
  (define (PT c)
    (or (< x (square c))
	(and (not (= (euclidean-remainder x c) 0))
	     (PT (+ c 2)))))
  (or (= x 2)
      (and (not (< x 2))
	   (not (= (euclidean-remainder x 2) 0))
	   (PT 3))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
	(cadr pair)
	(+ (car pair)
	   (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap (lambda (i)
			  (map (lambda (j) (list i j))
			       (enumerate-interval 1 (1- i))))
			(enumerate-interval 1 n)))))

(define (remove item s)
  (filter (lambda (x)
	    (not (= x item)))
	  s))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p)
			(cons x p))
		      (permutations (remove x s))))
	       s)))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list j i))
		  (enumerate-interval 1 (1- i))))
	   (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

;; Exercise 2.41
(define (unique-triplets n)
  (flatmap (lambda (i)
	     (map (lambda (p)
		    (append p (list i)))
		  (unique-pairs (1- i))))
	   (enumerate-interval 3 n)))

(define (given-sum-triplets n s)
  (map (lambda (t)
	 (append t
		 (list s)))
       (filter (lambda (t)
		 (= (+ (car t)
		       (cadr t)
		       (caddr t))
		    s))
	       (unique-triplets n))))

;; Exercise 2.42
(define empty-board '())

(define (make-position row column)
  (cons row column))

(define (position-row position)
  (car position))

(define (position-column position)
  (cdr position))

(define (first-position-row positions)
  (position-row (car positions)))

(define (first-position-column positions)
  (position-column (car positions)))

(define (safe? column positions)
  (define (S row rest)
    (cond ((null? rest) #t)
	  ((or (= (first-position-row rest) row)
	       (= (abs (- (first-position-row rest) row))
		  (- column (first-position-column rest))))
	   #f)
	  (else (S row (cdr rest)))))
  (S (caar positions) (cdr positions)))

(define (adjoin-position row column rest-of-queens)
  (cons (make-position row column) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position
		    new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (1- k))))))
  (queen-cols board-size))
