#lang sicp

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    ( - x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point f first-guess)
  (define (close-enough? a b) (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define tolerance 0.001)

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 1 2 3) 1.0)

(define (double f) (lambda (x) (f (f x))))
(((double (double double)) inc) 5)

(define (square x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))
((compose square inc) 6)

(define (repeated f n)
  (lambda (x)
    (define (iter arg k)
      (if (= k 1)
          (f arg)
          (iter (f arg) (- k 1))))
    (iter x n)))

((repeated square 2) 5)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-repeated f n) ((repeated smooth n) f))

((smooth square) 10)
((smooth-repeated square 1) 10)
((smooth-repeated square 3) 10)
((smooth (smooth (smooth square))) 10)
((smooth-repeated square 10) 10)
((smooth-repeated square 10) 0.5)
((smooth-repeated square 10) 1)

(define (iterative-improve check improve)
  (lambda (start-guess)
    (define (iter guess)
      (if (check guess)
          guess
          (iter (improve guess))))
    (iter start-guess)))

(define (fixed-point-2 f first-guess)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (f guess) guess)) tolerance))
                      f)
   first-guess))

(define (cube-root x)
  (fixed-point-2 (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (average x y) (/ (+ x y) 2))
(define (average-damp f) (lambda (x) (average x (f x))))

(cube-root 10)