#lang sicp
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (scale-tree1 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree1 sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree1 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree1 sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(square-tree1 nil)

(define (tree-map proc tree)
     (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map proc sub-tree)
                (proc sub-tree)))
          tree))

(define (square-tree2 tree) (tree-map (lambda (x) (* x x)) tree))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (tail) (cons (car s) tail)) rest)))))

(subsets '(1 2 3))