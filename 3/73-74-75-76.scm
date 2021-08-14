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

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

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

(define (stream-map-comp proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-comp
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map-comp + s1 s2))

;;интегрирование
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; 3.73
(define ones (cons-stream 1 ones))

(define (RC R C dt)
  (lambda (u0 i)
    (add-streams (integral (scale-stream i (/ 1 C)) u0 dt)
                 (scale-stream i R))))

(define RC1 (RC 5 1 0.5))

(display "RC-circuit:")
(display-stream-part (RC1 0.2 ones) 10)

;; 3.74

(define (sign-change-detector next start)
  (cond ((and (< start 0) (> next 0)) 1)
        ((and (> start 0) (< next 0) -1))
        (else 0)))

(define (get-rand-series)
  (cons-stream (- (random 10) 5) (get-rand-series)))

(define sense-data (get-rand-series))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings1 (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map-comp sign-change-detector sense-data (cons-stream 0 sense-data)))

(display "\nSense data:")
(display-stream-part sense-data 11)
(display "\nZero crossing 1:")
(display-stream-part zero-crossings1 10)
(display "\nZero crossing 1b:")
(display-stream-part zero-crossings 10)

;; 3.75
(define (make-zero-crossing-2 input-stream last-value prev)
  (let ((apvt (/ (+ (stream-car input-stream) prev) 2)))
    (cons-stream (sign-change-detector apvt last-value)
                 (make-zero-crossing-2 (stream-cdr input-stream) apvt (stream-car input-stream)))))

(define zero-crossings-2 (make-zero-crossing-2 sense-data 0 0))
(display "\nZero crossing 2:")
(display-stream-part zero-crossings-2 10)

;; 3.76
(define (smooth s)
  (cons-stream (* (stream-car s) 0.5)
               (scale-stream (add-streams s (stream-cdr s)) 0.5)))

(display "\nSmoothed:")
(display-stream-part (smooth sense-data) 11)

(define zero-crossings-3 (make-zero-crossings (smooth sense-data) 0))
(display "\nZero crossing 3:")
(display-stream-part zero-crossings-3 10)