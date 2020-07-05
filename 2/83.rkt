#lang racket
;; operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(put 'raise 'integer
     (lambda (x) (make-rational x 1)))

(put 'raise 'rational
     (lambda (x) (make-real (numer x) (denom x))))

(put 'raise 'real
     (lambda (x) (make-complex-from-real-imag x 0)))

(define (raise x)
  (apply-generic 'raise x))