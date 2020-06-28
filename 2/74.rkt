#lang racket
;; operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
          (apply proc (map contents args))
          (error
           "Нет метода для этих типов -- APPLY-GENERIC"
           (list op type-tags))))))

;; typization support
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Некорректные помеченные данные -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Некорректные помеченные данные -- CONTENTS" datum)))

(define (get-record file)
  (apply-generic 'get-record file))
(define (get-salary file employee)
  (apply-generic 'get-salary file employee))
(define (find-employee-record employee-name . files)
  (if (= 0 (length files))
      '()
      (let ((next-try (apply-generic 'get-employee-by-name (car files) employee-name)))
        (if (null? next-try)
            (find-employee-record employee-name (cdr files))
            next-try))))