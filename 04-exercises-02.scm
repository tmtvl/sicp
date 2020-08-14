;; Exercise 1.7
(define (average x y)
  (/ (+ x y) 2))

(define (improve x y)
  (average x (/ y x)))

(define (good-enough? x y)
  (< (abs (- x (improve x y)))
     (/ x 1000000)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter
       (improve guess x)
       x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.8
(define (cube-good-enough? guess x)
  (< (abs (- guess
	     (/ x
		(* guess guess))))
     0.001))

(define (cube-improve guess x)
  (/ (+
      (/ x (* guess guess))
      (* 2 guess))
     3))

(define (cube-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-iter
       (cube-improve guess x)
       x)))

(define (cube-root x)
  (cube-iter 1.0 x))
