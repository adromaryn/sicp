#lang sicp
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carm-test-all n)
    (define (try-it a)
        (= (expmod a n n) a))
    (define (test i)
        (cond ((= i 1) true)
              ((try-it i) (test (- i 1)))
              (else false)))
    (if (> n 1)
        (test (- n 1))
        false))

(carm-test-all 1000000)

(carm-test-all 1000003)
(carm-test-all 1000033)
(carm-test-all 1000037)

(carm-test-all 561)
(carm-test-all 1105)
(carm-test-all 1729)
(carm-test-all 2465)
(carm-test-all 2821)
(carm-test-all 6601)
