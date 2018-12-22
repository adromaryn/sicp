#lang sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (cube x) (* x x x))

(define (next x) (+ x 2))

(sum cube 0 next 4)