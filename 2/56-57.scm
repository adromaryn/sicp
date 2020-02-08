#lang scheme

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum-iter accum . l0)
  (let ((number (car accum))
        (sum (cdr accum))
        (l (car l0)))
    (if (null? l)
        (cond ((null? sum) number)
              ((and (= number 0)
                    (> (length sum) 1))
               (cons '+ sum))
              ((= number 0) (car sum))
              (else (cons '+ accum)))
        (let ((next (car l))
              (tail (cdr l)))
          (cond ((number? next) (make-sum-iter
                                 (cons (+ number next) sum)
                                 tail))
                (else (make-sum-iter
                       (cons number (cons next sum))
                       tail)))))))

(define (make-sum a . l)
  (make-sum-iter '(0) (cons a l)))

(define (make-product-iter accum . l0)
  (let ((number (car accum))
        (prod (cdr accum))
        (l (car l0)))
    (if (null? l)
        (cond ((null? prod) number)
              ((= number 0) 0)
              ((null? prod) number)
              ((and (= number 1)
                    (> (length prod) 1))
               (cons '* prod))
              ((= number 1) (car prod))
              (else (cons '* accum)))
        (let ((next (car l))
              (tail (cdr l)))
          (cond ((number? next) (make-product-iter
                                 (cons (* number next) prod)
                                 tail))
                (else (make-product-iter
                       (cons number (cons next prod))
                       tail)))))))

(define (make-product a . l)
  (make-product-iter '(1) (cons a l)))

(define (make-exponentiation a n)
  (cond ((= n 0) 1)
        ((= n 1) a)
        (else (list '** a n))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (> (length (cddr s)) 1)
      (cons '+ (cddr s))
      (caddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (> (length (cddr p)) 1)
      (cons '* (cddr p))
      (caddr p)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**) (number? (caddr x))))
(define (base x) (caddr x))
(define (exponent x) (cadr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-exponentiation (exponent exp) (- (base exp) 1))
          (make-product
           (deriv (exponent exp) var)
           (base exp))))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ (* y x) (** x 5)) 'x)
(deriv '(* x (** x 2)) 'x)
(deriv '(* x (** x 1)) 'x)

(make-sum 1 2 3 'x 4 0 10)

(deriv '(* x (+ 1 2 3 x 4)) 'x)

(make-product 1 2 3 'x 4 0 10 'y)
(make-product 1 2 3 'x 4 1 10 'y)

(deriv '(* x y 10) 'x)
(deriv '(* x y (+ 1 2 3 x 4)) 'x)