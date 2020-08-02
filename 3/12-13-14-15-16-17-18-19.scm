#lang sicp

(display "3.12:\n")
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x '(a b))
(define y '(c d))
(define z (append x y))
z
(cdr x)

(define w (append! x y))

w

(cdr x)

(display "3.13 cycle:\n")
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define c (make-cycle '(a b c)))
c
;; (last-pair c) - infinite recursion

(display "Mystery:\n")
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
;; reverse
(mystery '(1 2 3 4 5))

(define v '(a b c d))
(define w2 (mystery v))
v
w2

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(display "Разделение пар:\n")
(define xl '(a b))
(define z1 (cons xl xl))
(define z2 (cons '(a b) '(a b)))
(set-to-wow! z1)
(set-to-wow! z2)

(display "Подсчёт пар:\n")
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs '(1))
(count-pairs '(1 2))
(count-pairs '(1 2 3))
(count-pairs '((1) 2 3))
(define xc '(1))
(define yc (list xc xc))
yc
(count-pairs yc)
(define zc (list xc xc))
(define xc2 (cons xc xc))
(define yc2 (cons xc2 xc2))
yc2
(count-pairs yc2)
(define y-cycled (make-cycle '(a b c)))
y-cycled
;; (count-pairs y-cycled) - infinite recursion

(display "Подсчёт пар с учетом ссылок:\n")
(define (count-pairs-ref x)
  (let ((counted '()))
    (define (count-scoped x)
      (begin
        (define (is-counted? x c)
          (cond ((null? c) #f)
                ((eq? (car c) x) #t)
                (else (is-counted? x (cdr c)))))
        (cond ((not (pair? x)) 0)
              ((is-counted? x counted) 0)
              (else
               (begin
                 (set! counted (cons x counted))
                 (+ (count-scoped (car x))
                    (count-scoped (cdr x))
                    1))))))
    (count-scoped x)))

(count-pairs-ref '(1))
(count-pairs-ref '(1 2))
(count-pairs-ref '(1 2 3))
(count-pairs-ref '((1) 2 3))
yc
(count-pairs-ref yc)
yc2
(count-pairs-ref yc2)
y-cycled
(count-pairs-ref y-cycled)
(list y-cycled y-cycled)
(count-pairs-ref (list y-cycled y-cycled))

(display "Ищем циклы:\n")
(define (is-cycle? x)
  (let ((counted '()))
    (begin
      (define (is-cycle-scoped? x)
        (define (is-counted? x c)
          (cond ((null? c) #f)
                ((eq? (car c) x) #t)
                (else (is-counted? x (cdr c)))))
        (cond ((not (pair? x)) #f)
              ((is-counted? x counted) #t)
              (else
               (set! counted (cons x counted))
               (is-cycle-scoped? (cdr x)))))
      (is-cycle-scoped? x))))

yc
(is-cycle? yc)
yc2
(is-cycle? yc2)
y-cycled
(is-cycle? y-cycled)
(list y-cycled y-cycled)
(is-cycle? (list y-cycled y-cycled)) ;; не цикличный, т.к. кончается завершается nil, а циклы элементы
(cons 'a y-cycled)
(is-cycle? (cons 'a y-cycled)) ;; цикличный, т.к. завершается циклом

(display "Ищем циклы без отжирания кучи памяти:\n")
(define (is-cycle-mem-safe? x)
  (define (is-cycle-iter? x tail)
    (cond ((or (null? tail) (null? (cdr tail))) #f)
          ((eq? x tail) #t)
          (else (is-cycle-iter? (cdr x) (cddr tail)))))
          
  (if (null? x)
      #f
      (is-cycle-iter? x (cdr x))))

'(1)
(is-cycle-mem-safe? '(1))
'(1 2)
(is-cycle-mem-safe? '(1 2))
'(1 2 3)
(is-cycle-mem-safe? '(1 2 3))
'(1 2 3 4)
(is-cycle-mem-safe? '(1 2 3 4))
yc
(is-cycle-mem-safe? yc)
yc2
(is-cycle-mem-safe? yc2)
y-cycled
(is-cycle-mem-safe? y-cycled)
(list y-cycled y-cycled)
(is-cycle-mem-safe? (list y-cycled y-cycled)) ;; не цикличный, т.к. кончается завершается nil, а циклы элементы
(cons 'a y-cycled)
(is-cycle-mem-safe? (cons 'a y-cycled)) ;; цикличный, т.к. завершается циклом