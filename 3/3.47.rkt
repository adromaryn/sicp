#lang racket

;; Реализация параллелизма как в SICP

(define (set-car! pair v)
  (set! pair (cons v (cdr pair))))

(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 '()))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

; No atomic, only for test!!!
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


;; Тест параллельного исполнения
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
x

(define x2 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x2 (* x2 x2))))
                  (s (lambda () (set! x2 (+ x2 1)))))
x2

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; semaphore with mutex (3.47 a)
(define (make-semaphore1 n)
  (let ((mutex (make-mutex))
        (count 0))
    (define (acquire)
      (mutex 'acquire)
      (if (= count n)
          (begin
            (mutex 'release)
            (acquire))
          (begin
            (set! count (+ 1 count))
            (mutex 'release)))
      'acquired)
    (define (release)
      (mutex 'acquire)
      (set! count (- count 1))
      (mutex 'release)
      'released)
    (define (the-sem m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)
            (else (error "Неизвестный метод -- MAKE-SEMAPHORE" m))))
    the-sem))

;; semaphore with test-and-set! (3.47 b)
(define (make-semaphore2 n)
  (let ((cell (list false))
        (counter 0))
    (define (acquire)
      (if (or (test-and-set! cell) (= counter n))
          (begin
            (clear! cell)
            (acquire))
          (begin
            (set! counter (+ counter 1))
            (clear! cell)
            'acquired)))
    (define (release)
      (if (test-and-set! cell)
          (begin
            (clear! cell)
            (release))
          (begin
            (set! counter (- counter 1))
            (clear! cell)
            'released)))
    (define (the-sem m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)
            (else (error "Неизвестный метод -- MAKE-SEMAPHORE" m))))
    the-sem))
  