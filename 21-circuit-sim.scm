(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (logical-and a1 a2)
  (cond ((and (= a1 1)
	      (= a2 1))
	 1)
	((and (or (= a1 0)
		  (= a1 1))
	      (or (= a2 0)
		  (= a2 1)))
	 0)
	(else (error "Invalid signal" a1 a2))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
				  (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or o1 o2)
  (cond ((and (= o1 0)
	      (= o2 0))
	 0)
	((and (or (= o1 1)
		  (= o1 0))
	      (or (= o2 1)
		  (= o2 0)))
	 1)
	(else (error "Invalid signal" o1 o2))))

;; Exercise 3.28
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1)
				 (get-signal o2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

;; Exercise 3.29
(define (or-gate2 o1 o2 output)
  (let ((n1 (make-wire))
	(n2 (make-wire))
	(a (make-wire)))
    (inverter o1 n1)
    (inverter o2 n1)
    (and-gate n1 n2 a)
    (inverter a output)))

;; Exercise 3.30
(define (ripple-carry-adder la lb ls c)
  (cond ((and (null? la)
	      (null? lb)
	      (null? ls))
	 (set-signal! c 0))
	((or (null? la)
	     (null? lb)
	     (null? ls))
	 (error "Mismatched wires: RIPPLE-CARRY-ADDER" la lb ls))
	(else
	 (let ((c-in (make-wire)))
	   (full-adder (car la)
		       (car lb)
		       c-in
		       (car ls)
		       c)
	   (ripple-carry-adder (cdr la)
			       (cdr lb)
			       (cdr ls)
			       c-in)))))

(define (ripple-carry-adder-delay n)
  (* n
     (+ (* inverter-delay 2)
	(* or-gate-delay 3)
	(* and-gate-delay 4))))


;; Representing wires
(define (make-wire)
  (let ((signal-value 0)
	(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
	(cons proc action-procedures)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
	     (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


;; The agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display " New-value = ")
		 (display (get-signal wire))
		 (newline))))

(define the-agenda (make-agenda))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)


;; Implementing the agenda
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

(define (make-agenda)
  (list 0))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

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

(define (empty-queue? queue)
  (null? (front-ptr queue)))

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

(define (print-queue queue)
  (display (front-ptr queue))
  (newline)
  (front-ptr queue))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr!
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda
			   (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))
