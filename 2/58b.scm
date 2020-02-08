#lang sicp

(define (var-unpack v)
  (if (pair? v)
      (car v)
      v))

(define (var-pack v)
  (if (pair? v)
      v
      (list v)))

(define (is-number? x)
  (or (number? x)
      (and (pair? x)
           (= (length x) 1)
           (number? (car x)))))

(define (=number? exp num)
  (and (number? (var-unpack exp)) (= (var-unpack exp) num)))

(define (variable? x)
  (or (symbol? x)
      (and (pair? x)
           (= (length x) 1)
           (symbol? (car x)))))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? (var-unpack v1) (var-unpack v2))))

(define (sum? x)
  (cond ((or
          (not (pair? x))
          (= (length x) 1))
         #f)
        ((eq? (cadr x) '+) #t)
        ((= (length x) 3) #f)
        (else (sum? (cddr x)))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (is-number? a1)
              (is-number? a2))
         (+ (var-unpack a1)
            (var-unpack a2)))
        (else (append (var-pack a1) (list '+) (var-pack a2)))))

(define (addend s)
  (if (eq? (cadr s) '+)
      (car s)
      (let ((rest (addend (cddr s))))
        (append (list (car s) (cadr s))
                (if (pair? rest)
                    rest
                    (list rest))))))

(define (product? p)
     (and (pair? p)
          (> (length p) 1)
          (eq? (cadr p) '*)
          (not (sum? (cddr p))))) 
            
(define (augend s)
  (if (eq? (cadr s) '+)
      (if (= (length (cddr s)) 1)
          (caddr s)
          (cddr s))
      (augend (cddr s))))

(define (make-product p1 p2)
  (cond ((or (=number? p1 0) (=number? p2 0)) 0)
        ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((and (is-number? p1)
              (is-number? p2))
         (* (var-unpack p1)
            (var-unpack p2)))
        (else (let ((pp1 (if (sum? p1)
                             (list p1)
                             p1))
                    (pp2 (if (sum? p2)
                             (list p2)
                             p2)))
                (append (var-pack pp1) (list '*) (var-pack pp2))))))

(define (multiplier p ) (car p))
(define (multiplicand p)
  (if (= (length (cddr p)) 1)
      (caddr p)
      (cddr p)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(deriv 'x 'x)

(deriv '(x * x) 'x)
(deriv '((x + 3) * x + y * (x + x + 1)) 'x)
