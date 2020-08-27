(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (aassoc key records)
  (cond ((null? records) '())
	((equal? key (caar records)) (car records))
	(else (aassoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value)
			(cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 local-table)))
	(if subtable
	    (let ((record (assoc key-2 subtable)))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 local-table)))
	(if subtable
	    (let ((record (assoc key-2 subtable)))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons
		       (list key-1
			     (cons key-2 value))
		       (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: MAKE-TABLE" m))))
    dispatch))

;; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key list)
      (cond ((null? list) #f)
	    ((same-key? key (caar list)) (car list))
	    (assoc key (cdr list))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 local-table)))
	(if subtable
	    (let ((record (assoc key-2 subtable)))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 local-table)))
	(if subtable
	    (let ((record (assoc key-2 subtable)))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons
		       (list key-1
			     (cons key-2 value))
		       (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: MAKE-TABLE" m))))
    dispatch))

;; Exercise 3.25
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key list)
      (cond ((null? list) #f)
	    ((same-key? key (caar list)) (car list))
	    (else (assoc key (cdr list)))))
    (define (lookup table keys)
      (if (null? keys)
	  table
	  (let ((subtable (assoc (car keys) (cdr table))))
	    (if subtable
		(lookup subtable (cdr keys))
		#f))))
    (define (build-table value keys)
      (cond ((null? keys) '())
	    ((null? (cdr keys)) (cons (car keys) value))
	    (else (list (car keys)
			(build-table value (cdr keys))))))
    (define (insert! table value keys)
      (if (null? keys)
	  (set-cdr! table value)
	  (let ((subtable (assoc (car keys) (cdr table))))
	    (if subtable
		(insert! subtable value (cdr keys))
		(set-cdr! table
			  (cons (build-table value keys)
				(cdr table)))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc)
	     (lambda (keys)
	       (lookup local-table keys)))
	    ((eq? m 'insert-proc!)
	     (lambda (value keys)
	       (insert! local-table value keys)))
	    (else (error "Unknown operation: MAKE-TABLE" m))))
    dispatch))
