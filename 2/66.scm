#lang scheme

(define (key x) x)

(define (lookup-unordered given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key set-of-records)
  (let ((e (entry set-of-records)))
    (cond ((null? set-of-records) false)
          ((= given-key (key e)) e)
          ((< given-key e) (lookup (left-branch set-of-records)))
          ((> given-key e) (lookup (right-branch set-of-records))))))
