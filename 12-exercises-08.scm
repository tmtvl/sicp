(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))

;; Exercise 2.1
(define (make-rat numer denom)
  (let* ((divisor (gcd numer denom))
	 (simple-numer (/ (abs numer)
			  (* divisor
			     (if (or (and (negative? numer)
					  (positive? denom))
				     (and (positive? numer)
					  (negative? denom)))
				 -1
				 1))))
	 (simple-denom (/ (abs denom)
			  divisor)))
    (cons simple-numer simple-denom)))

;; Exercise 2.2
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((sp (start-segment segment))
	(ep (end-segment segment)))
    (make-point (/ (+ (x-point sp)
		      (x-point ep))
		   2.0)
		(/ (+ (y-point sp)
		      (y-point ep))
		   2.0))))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ", ")
  (display (y-point point))
  (display ")")
  (newline))

;; Exercise 2.3
(define (make-rectangle diagonal)
  (cons (start-segment diagonal)
	(end-segment diagonal)))

(define (width-rectangle rectangle)
  (abs (- (x-point (car rectangle))
	  (x-point (cdr rectangle)))))

(define (height-rectangle rectangle)
  (abs (- (y-point (car rectangle))
	  (y-point (cdr rectangle)))))

(define (perimeter-rectangle rectangle)
  (* 2
     (+ (width-rectangle rectangle)
	(height-rectangle rectangle))))

(define (area-rectangle rectangle)
  (* (width-rectangle rectangle)
     (height-rectangle rectangle)))

(define (make-rectangle width height)
  (cons (make-point 0 0)
	(make-point width height)))

;; Exercise 2.4
(define (two-four-cons x y)
  (lambda (m) (m x y)))

(define (two-four-car z)
  (z (lambda (p q) p)))

(define (two-four-cdr z)
  (z (lambda (x y) y)))

;; Exercise 2.5
(define (two-five-cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (two-five-car z)
  (define (C a)
    (let ((next (expt 2 (1+ a))))
      (if (= (euclidean-remainder z next) 0)
	  (C (1+ a))
	  a)))
  (C 0))

(define (two-five-cdr z)
  (define (C d)
    (let ((next (expt 3 (1+ d))))
      (if (= (euclidean-remainder z next) 0)
	  (C (1+ d))
	  d)))
  (C 0))

;; Exercise 2.6
(define (zero f)
  (lambda (x) x))

(define (add-one n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (one f)
  (lambda (x)
    (f x)))

(define (two f)
  (lambda (x)
    (f (f x))))

;; Exercise 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
	       (lower-bound y)))
	(p2 (* (lower-bound x)
	       (upper-bound y)))
	(p3 (* (upper-bound x)
	       (lower-bound y)))
	(p4 (* (upper-bound x)
	       (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
		  (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; Exercise 2.8
(define (sub-interval x y)
  (let ((p1 (abs (- (lower-bound x)
		    (lower-bound y))))
	(p2 (abs (- (upper-bound x)
		    (upper-bound y)))))
    (make-interval (min p1 p2)
		   (max p1 p2))))

;; Exercise 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (width-interval-function f a b)
  (width-interval (f a b)))

(= (width-interval-function
    add-interval
    (make-interval 1 2)
    (make-interval 3 4))
   (width-interval-function
    add-interval
    (make-interval 3 4)
    (make-interval 5 6)))

(not (= (width-interval-function
	 mul-interval
	 (make-interval 1 2)
	 (make-interval 3 4))
	(width-interval-function
	 mul-interval
	 (make-interval 3 4)
	 (make-interval 5 6))))

;; Exercise 2.10
(define (div-interval x y)
  (if (or (= (lower-bound y) 0)
	  (= (upper-bound y) 0)
	  (and (negative? (lower-bound y))
	       (positive? (upper-bound y))))
      (error "Can't divide an interval that spans 0" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
		      (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
(define (mul-interval x y)
  (let ((lbx (lower-bound x))
	(ubx (upper-bound x))
	(lby (lower-bound y))
	(uby (upper-bound y)))
      (cond ((positive? lbx)
	     (make-interval (* lby
			       (if (positive? lby) lbx ubx))
			    (* uby
			       (if (negative? uby) lbx ubx))))
	    ((negative? ubx)
	     (make-interval (* uby
			       (if (negative? uby) ubx lbx))
			    (* lby
			       (if (positive? lby) ubx lbx))))
	    (else
	     (cond ((positive? lby)
		    (make-interval (* lbx ubx)
				   (* ubx ubx)))
		   ((negative? uby)
		    (make-interval (* ubx lby)
				   (* lbx lby)))
		   (else
		    (let ((l1 (* lbx uby))
			  (l2 (* ubx lby))
			  (u1 (* lbx lby))
			  (u2 (* ubx uby)))
		      (make-interval (min l1 l2)
				     (max u1 u2)))))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center-interval interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

;; Exercise 2.12
(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100)))

(define (percent-interval interval)
  (/ (width-interval interval)	(center-interval interval)))

;; Exercise 2.13
(< (abs (- (percent-interval (mul-interval
			      (make-percentage-interval 5.0 2)
			      (make-percentage-interval 5.0 2)))
	   (percent-interval (mul-interval
			      (make-percentage-interval 5.0 1)
			      (make-percentage-interval 5.0 3)))))
   0.01)
