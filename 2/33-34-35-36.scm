#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append '(1 2 3) '(4 5 6))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 (list 3 4) 5))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0 (map (
                        lambda (x)
                         (cond ((null? x) 0)
                               ((not (pair? x)) 1)
                               (else (count-leaves x)))) t)))
(count-leaves '(((1 2) 3 4) ((1 2) 3 4)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))


