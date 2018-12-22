#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))

  (define (term-k a b h)
    (define (g x)
      (define k (/ (- x a) h))
      (cond ((or (= k 0) (= k n)) (f x))
            ((even? k) (* 2 (f x)))
            (else (* 4 (f x)))))
    g)

  (* (/ h 3)
     (sum (term-k a n h) a (lambda (x) (+ x h)) b)))

(define (cube x) (* x x x))

(integral-simpson cube 0 1 100)