#lang sicp

(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define (fast-mul a b)
  (define (iter m a b)
    (cond ((= b 0) m)
          ((even? b) (iter m (double a) (halve b)))
          (else (iter (+ m a) (double a) (halve (- b 1))))))
  (iter 0 a b))

(fast-mul 2 10)
(fast-mul 3 10)
(fast-mul 2 10000)