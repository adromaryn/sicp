#lang sicp
(list 1 (list 2 (list 3 4)))
(list 1 2 3 4)
(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
x
(count-leaves x)

(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

(define (deep-reverse l)
  (cond ((null? l) l)
        ((pair? (car l)) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
        (else (append (deep-reverse (cdr l)) (list (car l))))))

(define x1 (list (list 1 2) (list 3 4)))
x1
(reverse x1)
(deep-reverse x1)

(define (fringe l)
  (cond ((pair? l) (append (fringe (car l)) (fringe (cdr l))))
        ((null? l) l)
        (else (list l))))
(fringe x1)