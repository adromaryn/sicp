#lang sicp
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((l1 (lower-bound x))
        (l2 (lower-bound y))
        (u1 (upper-bound x))
        (u2 (upper-bound y)))
    (cond ((and (> l1 0) (> l2 0))
           (make-interval (* l1 l2) (* u1 u2)))
          ((and (< u1 0) (< u2 0))
           (make-interval (* u1 u2) (* l1 l2)))
          ((and (> l1 0) (< l2 0) (> u2 0))
           (make-interval (* u1 l2) (* u1 u2)))
          ((and (< l1 0) (> u1 0) (> l2 0))
           (make-interval (* l1 u2) (* u1 u2)))
          ((and (< u1 0) (< l2 0) (> u2 0))
           (make-interval (* l1 u2) (* u1 l2)))
          ((and (< l1 0) (> u1 0) (< u2 0))
           (make-interval (* u1 l2) (* l1 u2)))
          ((and (< u1 0) (> l2 0))
           (make-interval (* l1 u2) (* u1 l2)))
          ((and (> l1 0) (< u2 0))
           (make-interval (* u1 l2) (* u2 l1)))
          ((and (< l1 0) (< l2 0) (> u1 0) (> u2 0))
           (make-interval (min (* l1 u2) (* u1 l2)) (max (* l1 l2) (* u1 u2) ))))))

(define (div-interval x y)
  (if (> (* (upper-bound y) (lower-bound y)) 0)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (display "Error: denom maybe 0")))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-center-percent center percent)
  (let ((radius (* center (/ percent 100))))
    (make-interval (- center radius) (+ center radius))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ((c (center i)))
    (* (/ (- (upper-bound i) c) c) 100)))

