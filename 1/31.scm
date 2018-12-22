#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (product term 1 next n))

(factorial 5)
(factorial 10)

(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (square x) (* x x))

(define (sum-pi n)
  (define (term x) x)
  (define (next x) (+ x 2))
  (cond ((= n 1) (/ 2 3))
        ((even? n) (/ (square (product term 2 next (+ 2 n)))
                      (square (product term 3 next (+ 1 n)))
                      2
                      (+ 2 n)))
        (else (/ (square (product term 2 next (+ 1 n)))
                 (square (product term 3 next (+ 2 n)))
                 2
                 (+ 2 n)))))

(* 4.0 (sum-pi 10000))