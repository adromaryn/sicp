#lang sicp
(define (cont-frac n d k)
  (define (cont-frac-part i)
    (if (= 1 i)
        (/ (n k) (d k))
        (let ((index (+ (- k i) 1)))
          (/ (n index) (+ (d index) (cont-frac-part (- i 1)))))))
  (cont-frac-part k))

(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 100)

(/  2 (+ 1 (sqrt 5)))

(define (cont-frac2 n d k)
  (define (cont-frac-iter i part)
    (let ((next-part (/ (n i) (+ (d i) part))))
      (if (= i 1)
          next-part
          (cont-frac-iter (- i 1) next-part))))
  (cont-frac-iter k 0))

(cont-frac2 (lambda (x) 1.0) (lambda (x) 1.0) 100)

( + 2 (cont-frac
       (lambda (x) 1.0)
       (lambda (x)
         (if (= (remainder x 3) 2)
             (/ (+ x 1) 1.5)
             1))
       100))

( + 2 (cont-frac2
       (lambda (x) 1.0)
       (lambda (x)
         (if (= (remainder x 3) 2)
             (/ (+ x 1) 1.5)
             1))
       100))

(define (tan-cf x k)
  (cont-frac
   (lambda (k)
     (if (= k 1)
         x
         (* -1 x x)))
   (lambda (k) (- (* 2 k) 1))
   k))

(define (tan-cf2 x k)
  (cont-frac2
   (lambda (k)
     (if (= k 1)
         x
         (* -1 x x)))
   (lambda (k) (- (* 2 k) 1))
   k))

(define pi 3.14159)
(tan-cf (/ pi 4) 100)
(tan-cf2 (/ pi 4) 100)