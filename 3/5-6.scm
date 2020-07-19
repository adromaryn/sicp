#lang scheme

(define (square x) (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (cesaro-test)
  (= (gcd (random 100500) (random 100500)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(estimate-pi 1000000)

(define (random-in-range low high)
  (let ((range (+ 1 (- high low))))
    (+ low (random range))))

(define (random-in-range-scale low high)
  (if (< (- high low) 1000)
      (/ (random-in-range (* 1000 low) (* 1000 high)) 1000)
       (random-in-range low high)))

(define (estimate-integral pred trials x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)
     1.0
     (monte-carlo trials (integral-test pred x1 x2 y1 y2))))

(define (integral-test pred x1 x2 y1 y2)
  (lambda () (pred (random-in-range-scale x1 x2) (random-in-range-scale y1 y2))))

(display "(x − 5)^2 + (y − 7)^2 ≤ 3^2:\n")

(estimate-integral (lambda (x y)
                     (<= (+ (square (- x 5))
                            (square (- y 7)))
                         9.0))
                   1000000 2 8 4 10)

(display "pi:\n")

(estimate-integral (lambda (x y) (<= (+ (square x) (square y)) 1.0))
                   1000000 -1 1 -1 1)

(define (rand-update x0)
  (modulo (+ (* 45 x0) 21) 67))

(define (rand-init) (current-seconds))

(define rand
  (let ((x (rand-init)))
    (lambda (command)
      (cond ((equal? command 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((equal? command 'reset)
             (lambda (new-init)
               (set! x new-init)))
            (else (error "Некорректная команда RAND"))))))

(display "Rand generate new cesaro\n")
(define (cesaro-test-2)
  (= (gcd (rand 'generate) (rand 'generate)) 1))

(define (estimate-pi-2 trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test-2))))

(estimate-pi-2 1000000)

(display "Rand generate\n")
(rand 'generate)
((rand 'reset) 100500)
(rand 'generate)
(rand 'generate)
