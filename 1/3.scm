#lang sicp

(define (square x) (* x x))

(define (pif a b c)
  (cond ((and (> a c) (> b c)) (+ (square a) (square b)))
        ((and (> a b) (> c b)) (+ (square a) (square c)))
        (else (+ (square b) (square c)))))

(pif 10 9 7)