#lang sicp

(define (last-pair l)
  (if (> (length l) 1)
      (last-pair (cdr l))
      l))

(last-pair (list 23 72 149 34))

(last-pair nil)

(define (reverse l)
  (if (> (length l) 1)
      (append (reverse (cdr l)) (list (car l)))
      l))
(reverse (list 1 4 9 16 25))

(define x1 (list (list 1 2) (list 3 4)))
x1
(reverse x1)
