(define (beside im1 im2)
  im1)

(define (below im1 im2)
  im1)

(define (flip-vert image)
  image)

(define (flip-horiz image)
  image)

(define (rotate180 image)
  (flip-vert (flip-horiz image)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (1- n))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (1- n)))
	    (right (right-split painter (1- n))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (1- n))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (1- n))))
	(below painter (beside smaller smaller)))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipper-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; Exercise 2.45
(define (split greater lesser)
  (define (S painter n)
    (if (= n 0)
	painter
	(let ((smaller (S painter (1- n))))
	  (greater painter (lesser smaller smaller)))))
  S)

(define right-split (split beside below))
(define up-split (split below beside))


;; Frames
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
	       (scale-vect (ycor-vect v) (edge2-frame frame))))))

;; Exercise 2.46
(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
	     (* (ycor-vect v) s)))

;; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge2-frame frame)
  (cddr frame))


;; Painters
(define (draw-line start end)
  (cons start end))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame)
	 (start-segment segment))
	((frame-coord-map frame)
	 (end-segment segment))))
     segment-list)))

;; Exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

;; Exercise 2.49
(define outline-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 0))
	 (make-segment (make-vect 1 0) (make-vect 1 1))
	 (make-segment (make-vect 1 1) (make-vect 0 1))
	 (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
	 (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
	 (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
	 (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
	 (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define wave-painter
  (segments->painter
   '()))

(define (end-segment segment)
  (cdr segment))
