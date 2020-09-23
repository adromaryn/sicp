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
              (cond ((and (null? last-keys) next-elem) (cdr next-elem))
                    (next-elem (apply lookup-internal (car next-elem last-keys)))
                    (else false)))))
      (apply lookup-internal (car (cdr local-table) keys)))

    (define (insert! value keys)
      (define (insert-internal! table keys front)
        (if (null? keys)
            (error "LOOKUP -- необходимы ключи")
            (let ((record (assoc (car keys) table))
                  (last-keys (cdr keys)))
              (cond (null?

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Неизвестная операция -- TABLE" m))))
    dispatch))