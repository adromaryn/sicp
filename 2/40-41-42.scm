#lang sicp
(#%require r5rs/init)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (foldl op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (foldl op initial (cdr sequence)))))

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

(define (flatmap proc seq)
  (foldl append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate? seq)
  (cond ((null? seq) nil)
        ((predicate? (car seq)) (cons (car seq) (filter predicate? (cdr seq))))
        (else (filter predicate? (cdr seq)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))
(prime-sum-pairs 10)

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(permutations '(1 2 3))

(define (uniq-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(uniq-pairs 4)

(define (prime-sum-pairs-2 n)
  (map make-pair-sum
       (filter prime-sum? (uniq-pairs n))))

(prime-sum-pairs-2 10)

(define (uniq-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j)) (uniq-pairs (- i 1))))
           (enumerate-interval 1 n)))
(uniq-triples 5)

(define (sum-triples n s)
  (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s)) (uniq-triples n)))
(sum-triples 5 9)

(define (queens board-size)
  (define empty-board nil)
  (define (safe? k positions)
    (define (safe-pair? m)
      (if (= m k)
          #t
          (let ((row (list-ref positions (- m 1)))
                (new-row (list-ref positions (- k 1))))
            (and (not (= row new-row))
                 (not (= (abs (- new-row row)) (- k m)))
                 (safe-pair? (+ m 1))))))
    (safe-pair? 1))
  (define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list new-row)))
      
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 1)
(queens 2)
(queens 3)
(queens 4)
(queens 8)