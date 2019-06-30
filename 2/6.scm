#lang sicp
(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f ( f x)))))
(define (+ x y)
  (lambda (f) (lambda (t) ((y f) ((x f) t)))))
(define (* x y)
  (lambda (f) (lambda (t) ((y (x f)) t))))

((zero inc) 0)
((one inc) 0)
((two inc) 0)
(display "addition:")
(newline)
(((+ zero zero) inc) 0)
(((+ zero one) inc) 0)
(((+ one two) inc) 0)
(((+ two one) inc) 0)
(display "multiple:")
(newline)
(((* zero two) inc) 0)
(((* one two) inc) 0)
(((* two one) inc) 0)
(((* two two) inc) 0)
