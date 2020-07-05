#lang racket
;; operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(put 'raise 'integer
     (lambda (x) (make-rational x 1)))

(put 'raise 'rational
     (lambda (x) (make-real (numer x) (denom x))))

(put 'raise 'real
     (lambda (x) (make-complex-from-real-imag x 0)))

(define (raise x)
  (apply-generic 'raise x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((x (car args))
                    (y (cadr args)))
                (cond ((raise-tower x y)
                       (apply-generic op (raise-tower x y) y))
                      ((raise-tower y x)
                       (apply-generic op x (raise-tower y x)))
                      (else (error "Невозможно выполнить процедуру для данных типов"))))
              (error "Невозможно выполнить процедуру для данных типов"))))))

(define (raise-tower x y)
  (let ((from-type (type-tag x))
        (to-type (type-tag y)))
    (if (equal? from-type to-type)
        x
        (let ((raised (raise x)))
          (if raised
              (raise-tower raised y)
              #f)))))


(put 'project 'complex
     (lambda (x) (make-real (real-part x))))
(put 'project 'real
     (lambda (x) 
       (let ((rational (inexact->exact x)))
         (make-rational (numerator rational)
                        (denominator rational)))))
(put 'project 'rational
     (lambda (x)
       (make-scheme-number (round (/ (numer x) (denom x))))))

(define (project x)
  (apply-generic 'project x))

(define (drop x)
  (let ((projected (project x)))
    (cond ((not projected) x)
          ((= (raise projected) x) (drop projected))
          (else x))))