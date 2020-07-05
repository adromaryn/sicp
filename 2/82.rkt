#lang racket
;; operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;; coercion table
(define *coercion-table* (make-hash))

(define (put-coercion c type proc)
  (hash-set! *coercion-table* (list c type) proc))

(define (get-coercion c type)
  (hash-ref *coercion-table* (list c type) '()))

;; typization support
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Некорректные помеченные данные -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-coerce op args type-tags type-tags)))))

(define (apply-coerce op args type-tags to-type-tags)
  (if (null? to-type-tags)
      (error "Невозможно вызвать процедуру для данных параметров")
      (let ((next-type (car to-type-tags))
            (rest-types (cdr to-type-tags)))
        (if (and (can-coerce-all? type-tags next-type)
                 (has-proc? op next-type (length type-tags)))
            (apply-generic op
                           (map (lambda (arg)
                                  (if (equal? (type-tag arg) next-type)
                                      arg
                                      ((get-coercion (type-tag arg) next-type) arg)))
                                args))
            (apply-coerce op args type-tags to-type-tags)))))

(define (can-coerce-all? types type)
  (if (null? types)
      #t
      (let ((coercion (get-coercion (car types) type)))
        (if coercion
            (can-coerce-all? (cdr types) type)
            #f))))

(define (has-proc? op type count)
  (let ((proc (get op (repeat type count))))
    (not (null? proc))))

(define (repeat elem n)
  (define (iter tail elem n)
    (if (= n 0)
        tail
        (iter (car elem tail) elem (- n 1))))
  (iter '() elem n))