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

;; Тест на поиске простых чисел
;;; Реализация проверки на простое число
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> ((lambda (x) (* x x)) test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

;; 3.50
(display-line "Задача 3.50")
(define (stream-map-comp proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-comp
              (cons proc (map stream-cdr argstreams))))))

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-map-comp
                  (lambda (x y) (- (* x y) 1))
                  (stream-enumerate-interval 1 1000000)
                  (stream-enumerate-interval 5 1000000)))))

;; 3.51
(display-line "Задача 3.51")
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 5)
(stream-ref x 7)

;; 3.52
(display-line "Задача 3.52")
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display-line sum)
(define y (stream-filter even? seq))
(display-line sum)
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display-line sum)
(stream-ref y 0)
(stream-ref y 1)
(stream-ref y 2)
(stream-ref y 3)
(stream-ref y 7)
(display-line sum)

;; Бесконечные потоки
(display-line "Бесконечные потоки")
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))
(display-line "100е число Фиббоначи ")
(stream-ref fibs 100)

;;; Решето Эрастофена
(define (sieve stream)
(cons-stream
 (stream-car stream)
 (sieve (stream-filter
         (lambda (x)
           (not (divisible? x (stream-car stream))))
         (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(display-line "50е простое число ")
(stream-ref primes 50)

;;; Неявное определение потоков
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map-comp + s1 s2))

(define integers2 (cons-stream 1 (add-streams ones integers2)))

(define fibs2
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs2)
                                         fibs2))))
(display-line "100е число Фиббоначи ")
(stream-ref fibs2 100)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))

(define primes2
  (cons-stream
   2
   (stream-filter prime-test? (integers-starting-from 3))))

(define (prime-test? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes2))
(define (square x) (* x x))

;; 3.53
(define s (cons-stream 1 (add-streams s s)))
(display-line "2^10 ")
(stream-ref s 10)

;; 3.54
(define (mul-streams s1 s2)
  (stream-map-comp * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials integers)))
(display-line "10! = ")
(stream-ref factorials 10)

;; 3.55
(define (partial-sums s)
  (define s2 (add-streams s (cons-stream 0 s2)))
  s2)

(display-line "Частичные суммы натуральных чисел")
(display-stream-part (partial-sums integers) 10)

;; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
(display-line "Ряд Хэмминга")
(display-stream-part S 20)

;; 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;;; 0.3333...
(define third (expand 1 3 10))
(display-line "1/3 = 0.")
(for-each (lambda (n) (display (stream-ref third n))) (enumerate-interval 0 20))

;; 3.59
;;; Коэффициенты интеграла многочлена
(define (div-streams s1 s2)
  (stream-map-comp / s1 s2))
(define (integrate-series s)
  (cons-stream
   (stream-car s)
   (div-streams (stream-cdr s) (stream-cdr integers))))

(display-line "Коэффициенты интеграла многочлена с коэф. 1")
(display-stream-part (integrate-series ones) 5)

;;; Разложения в ряд Тейлора
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(display-line "Коэффициенты экспоненты")
(display-stream-part exp-series 5)
(define cosine-stream
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-stream)))
(display-line "Коэффициенты косинуса")
(display-stream-part cosine-stream 5)
(display-line "Коэффициенты синуса")
(display-stream-part sine-series 5)

;; 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                                         (scale-stream (stream-cdr s2) (stream-car s1)))
                            (cons-stream 0 (mul-series (stream-cdr s1)
                                                       (stream-cdr s2))))))
(display-line "Перемножение многочленов")
(display-stream-part (mul-series integers integers) 5)

;; 3.61
(define (invert-unit-series s)
  (define inverted (cons-stream 1 (scale-stream (mul-series (stream-cdr s) inverted) -1)))
  inverted)

(display-line "Коэффициенты e^-x")
(display-stream-part (invert-unit-series exp-series) 5)

;; 3.62
(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "Нельзя делить на многочлен с 0 постоянным членом -- DIV-SERIES")
      (mul-series s1 (invert-unit-series (scale-stream s2 (/ 1 (stream-car s2)))))))

(define tan-series (div-series sine-series cosine-stream))
(display-line "Коэффициенты тангенса")
(display-stream-part tan-series 10)