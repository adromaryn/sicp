#lang sicp
(define (make-segment start end)
  (cons start end))

(define (start-point segment)
  (car segment))

(define (end-point segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (newline)
  (display "(")
  (print-point (start-point s))
  (display ",")
  (print-point (end-point s))
  (display ")"))

(define (square x) (* x x))

(define (length-segment s)
  (let ((start (start-point s))
        (end (end-point s)))
    (sqrt (+
           (square (- (x-point end) (x-point start)))
           (square (- (y-point end) (y-point start)))))))

(define A (make-point 1 -3))
(define B (make-point 2 4))
(print-segment (make-segment A B))

(define (make-rectangle1 seg1 length2 direction)
  (let ((x2 (+
             (/ (* (abs direction)
                   length2
                   (- (y-point (end-point seg1)) (y-point (start-point seg1))))
                (* direction (length-segment seg1)))
             (x-point (start-point seg1))))
        (y2 (+
             (/ (* (abs direction)
                   length2
                   (- (x-point (start-point seg1)) (x-point (end-point seg1))))
                (* direction (length-segment seg1)))
             (y-point (start-point seg1)))))
    (cons seg1 (make-segment (start-point seg1) (make-point x2 y2)))))

(define (make-rectangle2 seg1 seg2)
  (let ((seg2-moved (make-segment
                    (start-point seg1)
                    (make-point
                     (- (x-point (end-point seg2)) (- (x-point (start-point seg2)) (x-point (start-point seg1))))
                     (- (y-point (end-point seg2)) (- (y-point (start-point seg2)) (y-point (start-point seg1))))))))
    (cons seg1 seg2-moved)))

(define (get-rectangle-a rectangle) (car rectangle))
(define (get-rectangle-b rectangle) (cdr rectangle))
(define (req-p rectangle)
  (let ((a (get-rectangle-a rectangle))
        (b (get-rectangle-b rectangle)))
    (* (+ (length-segment a) (length-segment b)) 2)))
(define (req-square rectangle)
  (let ((a (get-rectangle-a rectangle))
        (b (get-rectangle-b rectangle)))
    (* (length-segment a) (length-segment b))))

(define req1 (make-rectangle1 (make-segment (make-point 1 2) (make-point 5 5)) (/ 20 3) 1))
(newline)
(req-square req1)
(req-p req1)

(define req2 (make-rectangle2
              (make-segment (make-point 1 2) (make-point 5 5))
              (make-segment (make-point -1 0) (make-point 3 (/ 16 3)))))
(newline)
(req-square req2)
(req-p req2)


