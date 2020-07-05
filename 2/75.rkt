#lang racket

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r f)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos f)))
          ((eq? op 'imag-part) (* r (sin f)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) f)
          (else
           (error "Неизвестная оп. -- MAKE-FROM-MAG-ANG" op))))
  dispatch)