#lang racket
;; needed for compatibility reasons
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))


;; basic function
(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

;; exact prime number test
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))


;; timed prime test
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))  ; takes time before function call

(define (start-prime-test n start-time)
  (if
    (prime? n)
    (report-prime (- (runtime) start-time))  ; calculates passed time since before function call
    (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; test
(timed-prime-test 243312543675863)
(newline)
(prime? 243312543675863)