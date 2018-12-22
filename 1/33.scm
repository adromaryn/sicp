#lang sicp
(define (filter-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n )
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square-sum a b)
  (define (plus x y) (+ x y))
  (define (next x) (+ x 1))
  (filter-accumulate plus 0 square a next b prime?))

(square-sum 2 10)
(square-sum 3 10)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-sum n)
  (define (mul x y) (* x y))
  (define (next x) (+ x 1))
  (define (self x) x)
  (define (gcd-n a) (= (gcd n a) 1))
  (filter-accumulate mul 1 self 1 next (- n 1) gcd-n))

(gcd-sum 10)
