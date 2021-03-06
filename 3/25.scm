#lang sicp
(define (make-table)
  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))

    (define (lookup . keys)
      (define (lookup-internal table . keys)
        (if (null? keys)
            (error "LOOKUP -- необходимы ключи")
            (let ((next-elem (assoc (car keys) table))
                  (last-keys (cdr keys)))
              (cond ((not next-elem) false)
                    ((null? last-keys) (cdr next-elem))
                    ((pair? (cdr next-elem)) (apply lookup-internal (cons (cdr next-elem) last-keys)))
                    (else false)))))
      (apply lookup-internal (cons (cdr local-table) keys)))

    (define (insert! value . keys)
      (define (make-subtable . keys)
        (let ((key (car keys))
              (last-keys (cdr keys)))
          (if (null? last-keys)
              (cons key value)
              (list key (apply make-subtable last-keys)))))
      (define (insert-internal! table . keys)
        (if (null? keys)
            (error "INSERT! -- необходимы ключи")
            (let ((record (assoc (car keys) (cdr table)))
                  (last-keys (cdr keys)))
              (cond ((and (null? last-keys) record) (set-cdr! record value))
                    (record (apply insert-internal! (cons record last-keys)))
                    (else (set-cdr! table
                                                 (cons
                                                  (apply make-subtable keys)
                                                  (cdr table))))))))
      (apply insert-internal! (cons local-table keys))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Неизвестная операция -- TABLE" m))))
    dispatch))

(define t (make-table))
((t 'insert!) 1 'one 'two 'three)
((t 'insert!) 2 'two 'one)
((t 'insert!) 4 'one 'two 'four)
((t 'lookup) 'one 'two 'three)
((t 'lookup) 'two 'one)
((t 'lookup) 'one 'two 'four)
((t 'lookup) 'one 'two 'two)
((t 'insert!) 100 'one 'two)
((t 'lookup) 'one 'two 'three)
((t 'lookup) 'two 'one)
((t 'lookup) 'one 'two 'four)
((t 'lookup) 'one 'two)