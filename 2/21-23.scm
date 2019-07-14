#lang sicp
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list1 (cdr items)))))

(square-list1 (list 1 2 3 4))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))

(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))