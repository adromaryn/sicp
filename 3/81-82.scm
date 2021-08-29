#lang scheme
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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (rand-update x0)
  (modulo (+ (* 45 x0) 21) 67))

;; пример метода Монте-Карло
(define (rand-init) (current-seconds))

(define random-numbers
  (cons-stream (rand-init)
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (if (= p 0) 1 (sqrt (/ 6 p))))
              (monte-carlo cesaro-stream 0 0)))
(stream-ref pi 100000)

;; 3.81
(define (rand commands)
  (let ((first-elem (cond ((and (pair? (stream-car commands))
                                (eq? (car (stream-car commands)) 'reset))
                           (cdr (stream-car commands)))
                           ((and (not (pair? (stream-car commands)))
                                 (eq? (stream-car commands) 'generate))
                            (rand-init))
                           (else (error "Only generate and reset commands")))))
    (define random-numbers
      (cons-stream first-elem
                   (stream-map (lambda (prev command)
                                 (cond ((and (pair? command)
                                             (eq? (car command) 'reset))
                                        (cdr command))
                                       ((and (not (pair? command))
                                             (eq? command 'generate))
                                        (rand-update prev))
                                       (else (error "Only generate and reset commands"))))
                               random-numbers (stream-cdr commands))))
    random-numbers))

(define commands
  (cons-stream 'generate (cons-stream (cons 'reset 3) (cons-stream 'generate (cons-stream 'generate commands)))))

(display-stream-part (rand commands) 15)


;; 3.82

(define (random-in-range low high)
  (let ((range (+ 1 (- high low))))
    (+ low (random range))))

(define (random-in-range-scale low high)
  (if (< (- high low) 1000)
      (/ (random-in-range (* 1000 low) (* 1000 high)) 1000)
       (random-in-range low high)))

(define (integral-test-stream pred x1 x2 y1 y2)
  (cons-stream (pred (random-in-range-scale x1 x2) (random-in-range-scale y1 y2))
               (integral-test-stream pred x1 x2 y1 y2)))

(define (estimate-integral pred x1 x2 y1 y2)
  (stream-map (lambda (m)
                (* (- x2 x1)
                   (- y2 y1)
                   1.0
                   m))
              (monte-carlo (integral-test-stream pred x1 x2 y1 y2) 0 0)))

(define (square x) (* x x))
(stream-ref (estimate-integral (lambda (x y)
                                 (<= (+ (square (- x 5))
                                        (square (- y 7)))
                                     9.0))
                               2 8 4 10) 1000)