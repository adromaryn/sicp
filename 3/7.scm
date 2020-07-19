#lang sicp

(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        (error "Недостаточно денег на счете")))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (let ((passwords (list secret)))
    (define (make-joint new-secret)
      (begin
        (set! passwords (cons new-secret passwords))
        dispatch))
    (define (check-pass s)
      (memq s passwords))
    (define (dispatch s m)
      (cond ((not (check-pass s)) (error "Неверный пароль"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'make-joint) make-joint)
            (else "Метод не найден")))
    dispatch))

(define (make-joint account secret new-secret)
  ((account secret 'make-joint) new-secret))

(define w1 (make-account 100 '12345))

((w1 '12345 'withdraw) 40)
(define w2 (make-joint w1 '12345 'qwerty))
((w2 'qwerty 'deposit) 50)
((w2 'qwerty1 'withdraw) 150)