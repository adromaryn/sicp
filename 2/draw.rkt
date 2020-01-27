#lang racket
(require sicp-pict)
(require racket/include)
(require "wave.rkt")
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(paint wave4)
(paint letterlambda)
(paint einstein)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4-2 (flipped-pairs wave))
(paint wave4-2)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 2))