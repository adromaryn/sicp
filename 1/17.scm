#lang sicp

(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

(fast-mul 2 10)
(fast-mul 3 10)
(fast-mul 2 10000)