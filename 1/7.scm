#lang sicp
(define (sqrt-iter prev guess x)
  (if (good-enough? prev guess)
      guess
      (sqrt-iter guess
                 (improve guess x)
                 x)))

(define (good-enough? prev guess)
  (< (/ (abs (- guess prev)) guess) tolerance))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.000001)

(define (sqrt x) (sqrt-iter -1.0 1.0 x))

(sqrt 10)
(sqrt 0.001)
(sqrt 100000)
    
