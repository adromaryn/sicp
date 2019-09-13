#lang sicp
(#%require r5rs/init)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(matrix-*-vector '((1 3 4) (2 5 6)) '(1 2 3))

(define (transpose mat)
  (accumulate-n cons nil mat))
(transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix '((1 2) (3 4)) '((5 6) (7 8)))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse1 '(1 2 3 4 5))
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse2 '(1 2 3 4 5))