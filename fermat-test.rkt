#lang racket
(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))
(define (halve n) (/ n 2))
(define (decrement n) (- n 1))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder
                   (square (expmod base (halve exp) m))
                   m))
    (else (remainder
            (* base (expmod base (decrement exp) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(fast-prime? 561 10)
(fast-prime? 562 10)
(fast-prime? 563 10)
(fast-prime? 564 10)
(fast-prime? 565 10)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if
    (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 561)
      