(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Front called on empty queue: FRONT-QUEUE" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "Delete called on empty queue: DELETE-QUEUE!" queue))
	(else (set-front-ptr! queue (cdr (front-ptr queue)))
	      queue)))

;; Exercise 3.21
(define (print-queue queue)
  (display (front-ptr queue))
  (newline)
  (front-ptr queue))

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    (else (error "Unknown operation: MAKE-QUEUE" m))))
    dispatch))

(define (front-ptr queue)
  (queue 'front-ptr))

(define (rear-ptr queue)
  (queue 'rear-ptr))

(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))

(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))

;; Exercise 3.23
;; (element previous next)
(define (make-dequeue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (lambda (m)
      (cond ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    (else (error "Unknown operation: MAKE-DEQUEUE" m))))))

(define (dq-front-ptr dequeue)
  (dequeue 'front-ptr))

(define (dq-rear-ptr dequeue)
  (dequeue 'rear-ptr))

(define (dq-set-front-ptr! dequeue item)
  ((dequeue 'set-front-ptr!) item))

(define (dq-set-rear-ptr! dequeue item)
  ((dequeue 'set-rear-ptr!) item))

(define (empty-deque? dequeue)
  (null? (dq-front-ptr dequeue)))

(define (front-deque dequeue)
  (if (empty-deque? dequeue)
      (error "Front called on empty dequeue: FRONT-DEQUE" dequeue)
      (car (dq-front-ptr dequeue))))

(define (rear-deque dequeue)
  (if (empty-deque? dequeue)
      (error "Rear called on empty dequeue: REAR!-DEQUE" dequeue)
      (car (dq-rear-ptr dequeue))))

(define (front-insert-deque! dequeue item)
  (let ((new-elt (cons item (cons '() (dq-front-ptr dequeue)))))
    (if (empty-deque? dequeue)
	(begin
	  (dq-set-front-ptr! dequeue new-elt)
	  (dq-set-rear-ptr! dequeue
			    (dq-front-ptr dequeue)))
	(begin
	  (set-car! (cdr (dq-front-ptr dequeue)) new-elt)
	  (dq-set-front-ptr! dequeue new-elt)))
    dequeue))

(define (rear-insert-deque! dequeue item)
  (let ((new-elt (list item (dq-rear-ptr dequeue))))
    (if (empty-deque? dequeue)
	(begin
	  (dq-set-front-ptr! dequeue new-elt)
	  (dq-set-rear-ptr! dequeue
			    (dq-front-ptr dequeue)))
	(begin
	  (set-cdr! (cdr (dq-rear-ptr dequeue)) new-elt)
	  (dq-set-rear-ptr! dequeue new-elt)))
    dequeue))

(define (front-delete-deque! dequeue)
  (if (empty-deque? dequeue)
      (error "Front delete called on empty dequeue: FRONT-DELETE-DEQUE"
	     dequeue)
      (begin
	(dq-set-front-ptr! dequeue (cddr (dq-front-ptr dequeue)))
	(set-car! (cdr (dq-front-ptr dequeue)) '())))
  dequeue)

(define (rear-delete-deque! dequeue)
  (if (empty-deque? dequeue)
      (error "Rear delete called on empty dequeue: REAR-DELETE-DEQUE"
	     dequeue)
      (begin
	(dq-set-rear-ptr! dequeue (cadr (dq-rear-ptr dequeue)))
	(set-cdr! (cdr (dq-rear-ptr dequeue)) '())))
  dequeue)

(define (print-deque dequeue)
  (define (dq-ptr-to-list dq-ptr)
    (if (null? dq-ptr)
	'()
	(cons (car dq-ptr)
	      (dq-ptr-to-list (cddr dq-ptr)))))
  (let ((lst (dq-ptr-to-list (dq-front-ptr dequeue))))
    (display lst)
    (newline)
    lst))
