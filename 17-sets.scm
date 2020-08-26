(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
  (define (U s)
    (cond ((null? s) set2)
	  ((element-of-set? (car s) set2)
	   (U (cdr s)))
	  (else (cons (car s) (U (cdr s))))))
  (if (null? set2)
      set1
      (U set1)))

;; Exercise 2.60
(define (adjoin-set x set)
  (cons x set))

(define (excise-from-set x set)
  (define (E s)
    (cond ((null? s) '())
	  ((equal? (car s) x) (cdr s))
	  (else (cons (car s) (E (cdr s))))))
  (E set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1)
				 (excise-from-set (car set1) set2))))
	(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (cons (car set1)
		    (union-set (cdr set1)
			       (excise-from-set (car set1) set2))))))


;; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1)
					  (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61
(define (adjoin-set x set)
  (define (A s)
    (cond ((null? s) (list x))
	  ((= x (car s)) s)
	  ((< x (car s)) (cons x s))
	  (else (cons (car s) (A (cdr s))))))
  (A set))

;; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 (else
		  (cons x2 (union-set set1 (cdr set2)))))))))


;; Sets as binary trees
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;; Exercise 2.65
(define (intersection-set set1 set2)
  (define (I s1 s2 sr)
    (if (or (null? s1)
	    (null? s2))
	sr
	(let ((x1 (entry s1))
	      (x2 (entry s2)))
	  (cond
	   ((= x1 x2)
	    (I (left-branch s1)
	       (left-branch s2)
	       (I (right-branch s1)
		  (right-branch s2)
		  (adjoin-set x1 sr))))
	   ((< x1 x2)
	    (I s1
	       (left-branch s2)
	       (I (right-branch s1)
		  s2
		  sr)))
	   (else
	    (I s1
	       (right-branch s2)
	       (I (left-branch s1)
		  s2
		  sr)))))))
  (I set1 set2 '()))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (adjoin-set (entry set1)
		  (union-set
		   (left-branch set1)
		   (union-set (right-branch set1) set2)))))


;; Sets and intformation retrieval
(define (key record)
  (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((record (entry set-of-records)))
	(cond ((= given-key (key record)) record)
	      ((< given-key (key record))
	       (lookup given-key (left-branch set-of-records)))
	      (else
	       (lookup given-key (right-branch set-of-records)))))))


;; Huffman Encoding Trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree
		    (make-leaf 'D 1)
		    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-result (decode sample-message sample-tree))

;; Exercise 2.68
(define (encode-symbol symbol tree)
  (define (E t p)
    (cond ((null? t) #f)
	  ((leaf? t)
	   (if (eq? (symbol-leaf t) symbol)
	       p
	       #f))
	  (else
	   (let ((lp (E (left-branch t)
			(append p (list 0)))))
	     (if lp
		 lp
		 (E (right-branch t)
		    (append p (list 1))))))))
  (let ((path (E tree '())))
    (if path
	path
	(error "Symbol not in tree: ENCODE-SYMBOL" symbol tree))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(equal? sample-message
	(encode sample-result sample-tree))

;; Exercise 2.69
(define (successive-merge pairs)
  (cond ((null? pairs) '())
	((null? (cdr pairs)) (car pairs))
	(else (successive-merge
	       (cons (make-code-tree (cadr pairs)
				     (car pairs))
		     (cddr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(equal? sample-tree
	(generate-huffman-tree '((A 4) (B 2) (D 1) (C 1))))

;; Exercise 2.70
(define rock-tree
  (generate-huffman-tree
   '((NA 16)
     (YIP 9)
     (SHA 3)
     (A 2)
     (GET 2)
     (JOB 2)
     (BOOM 1)
     (WAH 1))))

(define rock-song
  '(GET A JOB
	SHA NA NA NA NA NA NA NA NA
	GET A JOB
	SHA NA NA NA NA NA NA NA NA
	WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
	SHA BOOM))

(define encoded-rock-song
  (encode rock-song rock-tree))


;; Multiple representations for abstract data
(define (make-from-real-imag x y)
  (cons x y))

(define (real-part z)
  (car z))

(define (imag-part z)
  (cdr z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

(define (square x)
  (* x x))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-mag-ang r a)
  (cons (* r (cos a))
	(* r (sin a))))

(define (real-part z)
  (* (magnitude z)
     (cos (angle z))))

(define (imag-part z)
  (* (magnitude z)
     (sin (angle z))))

(define (magnitude z)
  (car z))

(define (angle z)
  (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z)
  (car z))

(define (imag-part-rectangular z)
  (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular
	      (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
	      (cons (* r (cos a))
		    (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
	      (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular z))
	((polar? z)
	 (real-part-polar z))
	(else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular z))
	((polar? z)
	 (imag-part-polar z))
	(else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular z))
	((polar? z)
	 (magnitude-polar z))
	(else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular z))
	((polar? z)
	 (angle-polar z))
	(else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; Exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp) var))))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (install-sum-package)
  (define (deriv exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  (define (addend s)
    (cadr s))
  (define (augend s)
    (define (recur l)
      (if (null? l)
	  '()
	  (make-sum (car l)
		    (recur (cdr l)))))
    (recur (cddr s)))
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
  (define (sum? exp)
    (and (pair? exp) (eq? (car exp) '+)))
  (put 'deriv '(+) deriv)
  (put 'addend '(+) addend)
  (put 'augend '(+) augend)
  (put 'make-sum'(+) make-sum)
  (put 'sum? '(+) sum?))

(define (install-product-package)
  (define (deriv exp var)
    (make-product (deriv (multiplier exp) var)
		  (deriv (multiplicand exp) var)))
  (define (multiplier s)
    (cadr s))
  (define (multiplicand s)
    (define (recur l)
      (if (null? l)
	  '()
	  (make-product (car l)
			(recur (cdr l)))))
    (recur (cddr s)))
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
  (define (product? exp)
    (and (pair? exp) (eq? (car exp) '*)))
  (put 'deriv '(*) deriv)
  (put 'multiplier '(*) multiplier)
  (put 'multiplicand '(*) multiplicand)
  (put 'make-product'(*) make-product)
  (put 'product? '(*) product?))

(define (install-exponentiation-package)
  (define (deriv exp var)
    ((get 'make-product '*)
     (exponent exp)
     ((get 'make-product '*)
      (make-exponentiation
       (base exp)
       ((get 'make-sum '+)
	(exponent exp) -1))
      (deriv (base exp) var))))
  (define (base s)
    (cadr s))
  (define (exponent s)
    (caddr s))
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
	  ((=number? b 1) b)
	  ((and (number? b)
		(number? e))
	   (expt b e))
	  (else (list '** b e))))
  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))
  (put 'deriv '(**) deriv)
  (put 'base'(**) multiplier)
  (put 'exponent'(**) multiplicand)
  (put 'make-exponentiation'(**) make-exponentiation)
  (put 'exponentiation? '(**) exponentiation?))


;; Message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude) (sqrt (+ (square x)
					(square y))))
	  ((eq? op 'angle) (atan y x))
	  (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg)
  (arg op))

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)
