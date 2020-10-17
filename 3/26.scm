#lang sicp
(define (make-table)
  (define (make-tree entry left right)
    (list entry left right))
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (set-entry! tree e) (set-car! tree e))

  (let ((local-table (list '*table*)))
    (define (key kv)
      (car kv))
    (define (value kv)
      (cdr kv))
    (define (make-kv k v)
      (cons k v))
    (define (insert-table! k v)
      (define (adjoin-table tree kv)
        (cond ((null? tree) (make-tree kv '() '()))
              ((= (key kv) (key (entry tree)))
               (make-tree
                kv
                (left-branch tree)
                (right-branch tree)))
              ((< (key kv) (key (entry tree)))
               (make-tree
                (entry tree)
                (adjoin-table (left-branch tree) kv)
                (right-branch tree)))
              (else
               (make-tree
                (entry tree)
                (left-branch tree)
                (adjoin-table (right-branch tree) kv)))))
      (set-cdr! local-table (adjoin-table (cdr local-table) (make-kv k v)))
      'ok)

    (define (lookup-table k)
      (define (find-key tree)
        (cond ((null? tree) false)
              ((= k (key (entry tree))) (value (entry tree)))
              ((< k (key (entry tree))) (find-key (left-branch tree)))
              (else (find-key (right-branch tree)))))
      (find-key (cdr local-table)))

    (define (dispatch m)
      (cond ((equal? m 'insert!) insert-table!)
            ((equal? m 'lookup) lookup-table)
            (else (error "METHOD NOT FOUND -- " m))))
    dispatch))

(define t (make-table))
((t 'insert!) 1 'one)
((t 'insert!) 5 'five)
((t 'insert!) 10 'ten)
((t 'insert!) 0 'zero)
((t 'insert!) 3 'three)
((t 'insert!) 2 'two)
((t 'insert!) 7 'seven)

((t 'lookup) 0)
((t 'lookup) 1)
((t 'lookup) 2)
((t 'lookup) 3)
((t 'lookup) 4)
((t 'lookup) 5)
((t 'lookup) 6)
((t 'lookup) 7)
((t 'lookup) 10)