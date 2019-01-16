#lang sicp
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                  (search f neg-point midpoint))
                 ((negative? test-value)
                  (search f midpoint pos-point))
                 (else midpoint))))))

(define (average a b) (/ (+ a b) 2))

(define (close-enough? a b) (< (abs (- a b)) 0.000001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else
           (error "У аргументов не разные знаки " a b)))))

(define root
  (half-interval-method (lambda (x) (+ (* x x x) (* 3 x x) -5)) -100.0 100.0))

(display root)
(newline)

((lambda (x) (+ (* x x x) (* 3 x x) -5)) root)

(half-interval-method sin 2.0 4.0)

(define tolerance 0.001)

(define (fixed-point f first-guess)
  (define (close-enough? a b) (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)