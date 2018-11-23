#lang sicp
(define tolerance 0.1)

(define (good-enough? prev guess)
  (< (abs (/ (- guess prev) guess)) tolerance))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (sqrt3-iter prev guess x)
  (if (good-enough? prev guess)
      guess
      (sqrt3-iter guess
                 (improve guess x)
                 x)))

(define (sqrt3 x) (sqrt3-iter 0 1.0 x))

(sqrt3 10)
(sqrt3 0.001)
(sqrt3 100000)
