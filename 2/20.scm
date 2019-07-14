#lang sicp
(define (same-parity x . z)
  (define (parity-filter l1 l2 r)
    (cond ((null? l2) l1)
          ((= (remainder (car l2) 2) r)
           (parity-filter (append l1 (list (car l2))) (cdr l2) r))
          (else (parity-filter l1 (cdr l2) r))))
  (parity-filter (list x) z (remainder x 2)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
           
