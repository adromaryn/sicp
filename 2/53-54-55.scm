#lang scheme

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)
(car '(a b c))
(cdr '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

;;; 2.53
(list 'a 'b 'c)
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;;; 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))

(equal? 'a 'b)
(equal? 'ab 'ab)
(equal? 'ab '(a b))
(equal? '(a b) '(a b))
(equal? '(a b) '(a b c))
(equal? '() '())
(pair? '())
(eq? '() '())

;;; 2.55
(car ''abracadabra)
(car (quote (quote abracadabra)))
''abracadabra