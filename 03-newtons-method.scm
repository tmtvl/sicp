(define (average x y)
  (/ (+ x y) 2))

(define (improve x y)
  (average x (/ y x)))

(define (good-enough? x y)
  (< (abs (- x (/ y x))) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter
       (improve guess x)
       x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
