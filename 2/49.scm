#lang racket
(require sicp-pict)

(define trace
  (segments->painter (list
                     (make-segment (make-vect 0 0) (make-vect 0 1))
                     (make-segment (make-vect 0 1) (make-vect 1 1))
                     (make-segment (make-vect 1 1) (make-vect 1 0))
                     (make-segment (make-vect 1 0) (make-vect 0 0)))))

(paint trace)

(define diags
  (segments->painter (list
                     (make-segment (make-vect 0 0) (make-vect 1 1))
                     (make-segment (make-vect 0 1) (make-vect 1 0)))))

(paint diags)

(define rhomb
  (segments->painter (list
                     (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                     (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                     (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                     (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))
(paint rhomb)

(define wave
  (segments->painter (list
                     (make-segment (make-vect 0.5 1) (make-vect 0.6 0.8))
                     (make-segment (make-vect 0.6 0.8) (make-vect 0.5 0.7))
                     (make-segment (make-vect 0.5 0.7) (make-vect 0.6 0.7))
                     (make-segment (make-vect 0.6 0.7) (make-vect 1 0.4))
                     (make-segment (make-vect 1 0.3) (make-vect 0.6 0.5))
                     (make-segment (make-vect 0.6 0.5) (make-vect 0.7 0))
                     (make-segment (make-vect 0.6 0) (make-vect 0.5 0.3))
                     (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0))
                     (make-segment (make-vect 0.2 0) (make-vect 0.4 0.5))
                     (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0.6))
                     (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.4))
                     (make-segment (make-vect 0.2 0.4) (make-vect 0 0.7))
                     (make-segment (make-vect 0 0.8) (make-vect 0.2 0.6))
                     (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.7))
                     (make-segment (make-vect 0.3 0.7) (make-vect 0.4 0.7))
                     (make-segment (make-vect 0.4 0.7) (make-vect 0.3 0.8))
                     (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1)))))
(paint wave)