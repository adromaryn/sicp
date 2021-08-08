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

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 0))

(display "Pairs:")
(display-stream-part (pairs integers integers) 10)

;; 3.67
(define (pairs-full s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     (pairs-full (stream-cdr s) (stream-cdr t))))))

(display "\nPairs full:")
(display-stream-part (pairs-full integers integers) 10)

;; 3.68
(define (pairs-wrong s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-wrong (stream-cdr s) (stream-cdr t))))

;; (pairs-wrong integers integers)

;; 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (interleave
                 (stream-map (lambda (x) (list (stream-car t) x))
                             (stream-cdr u))
                 (pairs (stream-cdr t) (stream-cdr u))))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(display "\nTriples:")
(display-stream-part (triples integers integers integers) 20)

;; 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1)
                                               (stream-cdr s2)
                                               weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(display "\nWeighted-pairs:")
(display-stream-part (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))) 20)

(display "\nWeighted-pairs (module not 2,3,5):")
(display-stream-part (stream-filter (lambda (x) (let ((s (+ (car x) (cadr x))))
                                                  (and (odd? s)
                                                       (not (= (modulo s 3) 0))
                                                       (not (= (modulo s 5) 0)))))
                                    (weighted-pairs integers
                                                    integers
                                                    (lambda (x)
                                                      (let ((i (car x))
                                                            (j (cadr x)))
                                                        (+ (* 2 i) (* 3 j) (* 5 i j))))))
                     25)

(define (weighted-triples s t u weight)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (merge-weighted
    (stream-map (lambda (x) (cons (stream-car s) x))
                (merge-weighted
                 (stream-map (lambda (x) (list (stream-car t) x))
                             (stream-cdr u))
                 (weighted-pairs (stream-cdr t) (stream-cdr u) weight)
                 weight))
    (weighted-triples (stream-cdr s) (stream-cdr t) (stream-cdr u) weight)
    weight)))

(display "\nWeighted-triples:")
(define (weight-rec x)
  (if (null? x) 0 (+ (car x) (weight-rec (cdr x)))))
(display-stream-part (weighted-triples integers integers integers weight-rec) 20)

;; 3.71 Ramanujan numbers
(define (sum-2-cubes x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i i) (* j j j))))

(define ramanujan-cubes
  (let ((cube-pairs (weighted-pairs integers
                                    integers
                                    sum-2-cubes)))
    (define (cubes-filter s)
      (let ((cur (stream-car s))
            (next (stream-car (stream-cdr s)))
            (next-next (stream-car (stream-cdr (stream-cdr s))))
            (tail (stream-cdr (stream-cdr (stream-cdr s)))))
        (if (and (= (sum-2-cubes cur) (sum-2-cubes next)) (not (= (sum-2-cubes cur) (sum-2-cubes next-next))))
            (cons-stream (list cur next (sum-2-cubes cur)) (cubes-filter tail))
            (cubes-filter (stream-cdr s)))))
    (cubes-filter cube-pairs)))

(display "\nRamanujan cubes:")
(display-stream-part ramanujan-cubes 10)

;; 3.72 Ramanujan squares
(define (sum-3-squares x)
  (let ((i (car x))
        (j (cadr x))
        (k (caddr x)))
    (+ (* i i) (* j j) (* k k))))

(define (weight-square-rec x)
  (if (null? x) 0 (+ (* (car x) (car x)) (weight-square-rec (cdr x)))))

(define ramanujan-squares
  (let ((square-pairs (weighted-triples integers
                                        integers
                                        integers
                                        weight-square-rec)))
    (define (square-filter s)
      (let ((cur (stream-car s))
            (next (stream-car (stream-cdr s)))
            (next-next (stream-car (stream-cdr (stream-cdr s))))
            (next-next-next (stream-car (stream-cdr (stream-cdr (stream-cdr s)))))
            (tail (stream-cdr (stream-cdr (stream-cdr s)))))
        (if (and (= (sum-3-squares cur) (sum-3-squares next) (sum-3-squares next-next)) (not (= (sum-3-squares cur) (sum-3-squares next-next-next))))
            (cons-stream (list cur next next-next (sum-3-squares cur)) (square-filter tail))
            (square-filter (stream-cdr s)))))
    (square-filter square-pairs)))

(display "\nRamanujan squares:")
(display-stream-part ramanujan-squares 10)