#lang scheme

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "плохой бит -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (element-of-set-1? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-1? x (cdr set)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (if (element-of-set-1? s (symbols tree))
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (element-of-set-1? s (symbols left))
            (encode-symbol-branch s left 0)
            (encode-symbol-branch s right 1)))
      (error "плохой символ -- ENCODE-SYMBOL" s)))

(define (encode-symbol-branch s branch bit)
  (if (leaf? branch)
      (list bit)
      (cons bit
            (let ((left (left-branch branch))
                  (right (right-branch branch)))
              (if (element-of-set-1? s (symbols left))
                  (encode-symbol-branch s left 0)
                  (encode-symbol-branch s right 1))))))

(encode '(A D A B B C A) sample-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge s)
  (if (= (length s) 1)
      (car s)
      (successive-merge (adjoin-set (make-code-tree (cadr s) (car s)) (cddr s)))))

(define alphabet '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
(define song-tree (generate-huffman-tree alphabet))
song-tree

(define song '(
               get a job
               sha na na na na na na na na
               get a job
               sha na na na na na na na na
               wah yip yip yip yip yip yip yip yip yip
               sha boom))
(encode song song-tree)