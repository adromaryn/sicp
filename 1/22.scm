#lang sicp
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n )
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (great-odd n)
  (if (even? n)
      (+ n 1)
      n))

(define (less-odd n)
  (if (even? n)
      (- n 1)
      n))

(define (search-for-primes a b)
  (define (search-iter a b)
    (timed-prime-test a)
    (if (= a b)
        (display "")
        (search-iter (great-odd (+ a 2)) (less-odd b))))
  (search-iter (great-odd a) (less-odd b)))



(search-for-primes 1000 1019)

(search-for-primes 10000 10037)

(search-for-primes 100000 100043)

(search-for-primes 1000000 1000037)

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)