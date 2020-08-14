;; Exercise 1.2
(/
 (+ 5 4
    (- 2
       (- 3
	  (+ 6
	     (/ 4 5)))))
 (* 3
    (- 6 2)
    (- 7 2)))

;; Exercise 1.3
(define (sum-of-largest-two-of-three-squares x y z)
  (cond
   ((and (< x y)
	 (< x z))
    (+ (* y y) (* z z)))
   ((< y z)
    (+ (* x x) (* z z)))
   (else (+ (* x x) (* y y)))))
