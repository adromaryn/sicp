#lang racket

(define (square x) (* x x))

;; operation table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
          (apply proc (map contents args))
          (error
           "Нет метода для этих типов -- APPLY-GENERIC"
           (list op type-tags))))))

;; typization support
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Некорректные помеченные данные -- CONTENTS" datum))))

;; common ops
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (invert x) (apply-generic 'invert x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'invert '(scheme-number)
       (lambda (x) (* -1 x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x) (= (numer x) 0))
  (define (invert x) (make-rat (* -1 (numer x)) (denom x)))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'invert '(rational) invert)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; процедуры, импортируемые из декартова
  ;; и полярного пакетов
  (install-rectangular-package)
  (install-polar-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; внутренние процедуры
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (eq-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;; интерфейс к остальной системе
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put '=zero? '(complex) =zero?)
  (put 'invert '(complex) invert)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) eq-complex?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

;; rectangular representation 
(define (install-rectangular-package)
  ;; внутренние процедуры
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (=zero? z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))
  (define (invert z)
    (make-from-real-imag (* -1 (real-part z))
                         (* -1 (imag-part z))))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put '=zero? '(rectangular) =zero?)
  (put 'invert '(rectangular) invert)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; внутренние процедуры
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (=zero? z)
    (= 0 (magnitude z)))
  (define (invert z)
    (make-from-mag-ang (* -1 (magnitude z))
                       (angle z)))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put '=zero? '(polar) =zero?)
  (put 'invert '(polar) invert)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;; Poly package
(define (install-polynomial-package)

  (install-sparse-terms-package)
  (install-dense-terms-package)
;; внутренние процедуры
;; представление poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-sparse L)
    ((get 'make 'sparse) L))
  (define (make-dense L)
    ((get 'make 'dense) L))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Многочлены от разных переменных -- ADD-POLY"
               (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1 (invert-poly p2))
        (error "Многочлены от разных переменных -- SUB-POLY"
               (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Многочлены от разных переменных -- MUL-POLY"
               (list p1 p2))))

  (define (=zero-poly? p)
    (=zero? (term-list p)))
  (define (invert-poly p)
    (make-poly (variable p) (invert (term-list p))))
        

  ;; интерфейс к остальной системе
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make-sparse 'polynomial
       (lambda (var terms) (tag (make-poly var (make-sparse terms)))))
  (put 'make-dense 'polynomial
       (lambda (var terms) (tag (make-poly var (make-dense terms)))))
  (put '=zero? 'polynomal =zero-poly?)
  (put 'invert '(polynomal) invert-poly)

  'done)

(define (install-sparse-terms-package)

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (=zero-term? terms)
    (if (empty-termlist? terms)
        #t
        (and (=zero? (coeff (first-term terms)))
             (=zero-term? (rest-terms)))))

  (define (invert-terms terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (let ((t (first-term terms)))
          (adjoin-term
           (make-term (order t)
                      (invert (coeff t)))
           (invert-terms (rest-terms terms))))))

  ;; интерфейс к остальной системе
  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(sparse sparse)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'make 'sparse (lambda (L) (tag L)))
  (put '=zero? 'sparse =zero-term?)
  (put 'invert '(sparse) (lambda (L) (tag (invert-terms L))))

  'done)

(define (install-dense-terms-package)

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (adjoin-term term term-list)
        (cons term term-list))

  (define (add-terms-with-zeroes L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (length L1) (length L2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (length L1) (length L2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (add t1 t2)
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (remove-zeros L)
    (cond ((null? L) '())
          ((= 0 (first-term L)) (remove-zeros (rest-terms L)))
          (else L)))
  
  (define (add-terms L1 L2)
    (remove-zeros (add-terms-with-zeroes L1 L2)))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first L1) L2 (- (length L1) 1))
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L range1)
    (define (add-range p r zeros)
      (if (= 0 r)
          (append p zeros)
          (add-range p (- r 1) (cons 0 zeros))))
    (add-range
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term (mul t1 t2)
                         (mul-term-by-all-terms t1 (rest-terms L) 0))))
      range1 '()))

  (define (invert-terms terms)
    (map invert terms))

  ;; интерфейс к остальной системе
  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(dense dense)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'make 'dense (lambda (L) (tag L)))
  (put '=zero? 'dense empty-termlist?)
  (put 'invert '(dense) (lambda (L) (tag (invert-terms L))))

  'done)
  

(install-polynomial-package)

(define (make-poly-sparse var terms)
  ((get 'make-sparse 'polynomial) var terms))
(define (make-poly-dense var terms)
  ((get 'make-dense 'polynomial) var terms))

(display "\nПроверка разреженного представления:\n")
(define x (make-poly-sparse 'x
                            '((10 -3)
                              (5 2)
                              (0 1))))

(define y (make-poly-sparse 'x
                            '((7 2)
                              (5 -1)

                              (1 2))))

(display "Исходные данные:\n")
x
y
(display "Сумма:\n")
(add x y)
(display "Разность:\n")
(sub x y)
(display "Произведение:\n")
(mul x y)


(display "\nПроверка плотного представления:\n")
(define x2 (make-poly-dense 'x '(1 -2 3 4)))
(define y2 (make-poly-dense 'x '(1 -2 3)))
(display "Исходные данные:\n")
x2
y2
(display "Сумма:\n")
(add x2 y2)
(display "Разность:\n")
(sub x2 y2)
(display "Произведение:\n")
(mul x2 y2)