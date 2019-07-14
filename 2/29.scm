#lang sicp
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch structure)
  (list-ref structure 0))
(define (right-branch structure)
  (list-ref structure 1))

(define (branch-length branch)
  (list-ref branch 0))
(define (branch-structure branch)
  (list-ref branch 1))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))
(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (total-weight s)
        s)))

(define x
  (make-mobile (make-branch 2 (make-mobile (make-branch 1 1) (make-branch 1 2))) (make-branch 3 5)))
(total-weight x)

(define (balanced mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
    (and (= (* (branch-weight l) (branch-length l)) (* (branch-weight r) (branch-length r)))
         (or (not (pair? (branch-structure l))) (balanced (branch-structure l)))
         (or (not (pair? (branch-structure r))) (balanced (branch-structure r))))))

(balanced x)
(define y
  (make-mobile (make-branch 4 (make-mobile (make-branch 2 2) (make-branch 1 3))) (make-branch 5 4)))
(balanced y)
(define z
  (make-mobile (make-branch 4 (make-mobile (make-branch 3 2) (make-branch 2 3))) (make-branch 5 4)))
(balanced z)

(define (make-mobile2 left right)
  (cons left right))

(define (make-branch2 length structure)
  (cons length structure))

(define (left-branch2 structure)
  (car structure))
(define (right-branch2 structure)
  (cdr structure))

(define (branch-length2 branch)
  (car branch))
(define (branch-structure2 branch)
  (cdr branch))