#lang racket
(require sicp-pict)
(require racket/include)
(require "wave.rkt")
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(paint wave4)
(paint letterlambda)
(paint einstein)