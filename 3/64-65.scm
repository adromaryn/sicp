#lang sicp

;; Определение отложенных списков 
(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;; Операции для работы с отложенными списками
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter predicate? seq)
  (cond ((stream-null? seq) the-empty-stream)
        ((predicate? (stream-car seq)) (cons-stream (stream-car seq) (stream-filter predicate? (stream-cdr seq))))
        (else (stream-filter predicate? (stream-cdr seq)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (display-stream s)
  (stream-for-each display-line s))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons
       low
       (enumerate-interval (+ low 1) high))))

(define (display-stream-part s n)
  (for-each (lambda (n) (display-line (stream-ref s n))) (enumerate-interval 0 n)))

(define (display-line x)
  (newline)
  (display x))

;; SQRT example
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average a b) (/ (+ a b) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(display-line "Корни 2")
(display-stream-part (sqrt-stream 2) 10)

;; Вычисление Pi
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (partial-sums s)
  (define s2 (add-streams s (cons-stream 0 s2)))
  s2)

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(display-line "Вычисление Пи")
(display-stream-part pi-stream 20)

;; Ускорение знакопеременного ряда методом Эйлера
(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-line "Вычисление Пи ускореное")
(display-stream-part (euler-transform pi-stream) 20)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-line "Вычисление Пи сверхускоренное")
(display-stream-part (accelerated-sequence euler-transform
                                           pi-stream)
                     20)

;; 3.64
(define (stream-limit s tolerance)
  (define diffs
    (add-streams (stream-cdr s) (scale-stream s -1)))
  (define (stream-limit-search s d)
    (if (<= (stream-car d) tolerance)
        (stream-car (stream-cdr s))
        (stream-limit-search (stream-cdr s) (stream-cdr d))))
  (stream-limit-search s diffs))

(define (sqrt2 x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(newline)
(display "Корень 2: ")
(display-line (sqrt 2))
(display-line (sqrt2 2 0.00001))

;;3.65
(display-line "Вычисление ln 2:")

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2
  (partial-sums (ln2-summands 1)))
(display-stream-part ln2 20)

(display-line "Вычисление ln 2 ускореное")
(display-stream-part (euler-transform ln2) 20)

(display-line "Вычисление ln 2 сверхускоренное")
(display-stream-part (accelerated-sequence euler-transform
                                           ln2)
                     20)

