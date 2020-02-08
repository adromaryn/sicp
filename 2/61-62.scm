#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set s x)
  (cond ((null? s) (list x))
        ((< x (car s)) (cons x s))
        ((= x (car s)) s)
        (else (cons (car s) (adjoin-set (cdr s) x)))))

(define (union-set s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2) (cons x1 (union-set (cdr s1)  (cdr s2))))
              ((< x1 x2) (union-set (cdr s1) s2))
              (else (union-set s1 (cdr s2)))))))
