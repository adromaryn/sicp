#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner a (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (define (c x y) (+ x y))
  (accumulate c 0 term a next b))

(define (product term a next b)
  (define (c x y) (* x y))
  (accumulate c 1 term a next b))

(define (term x) x)
(define (next x) (+ 1 x))

(sum term 1 next 5)
(product term 1 next 5)

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner a result))))
  (iter a null-value))

(define (sum2 term a next b)
  (define (c x y) (+ x y))
  (accumulate c 0 term a next b))

(define (product2 term a next b)
  (define (c x y) (* x y))
  (accumulate c 1 term a next b))

(sum2 term 1 next 5)
(product2 term 1 next 5)