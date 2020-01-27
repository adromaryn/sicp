#lang racket
(require sicp-pict)

(define (split big-pos small-pos)
  (define (pos-draw painter n)
    (if (= n 0)
        painter
        (let ((smaller (pos-draw painter (- n 1))))
          (big-pos painter (small-pos smaller smaller)))))
  pos-draw)
    
(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

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
                     (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1))
                     (make-segment (make-vect 0.4 0.8) (make-vect 0.45 0.75))
                     (make-segment (make-vect 0.45 0.75) (make-vect 0.5 0.8)))))
(paint (corner-split wave 3))
(paint (square-limit wave 3))