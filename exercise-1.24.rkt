#lang racket
;; needed for compatibility reasons
(#%require (only racket/base current-inexact-milliseconds))
(define (runtime) (current-inexact-milliseconds))


;; basic function
(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? n) (divides? 2 n))

;; exact prime number test
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))


;; fermat test
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

;; timed prime test
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))  ; takes time before function call

(define (start-prime-test n start-time)
  (if
    (fast-prime? n 20)
    (report-prime (- (runtime) start-time))  ; calculates passed time since before function call
    (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; test
;(timed-prime-test 243312543675863)
;(newline)
;(prime? 243312543675863)

(define (search-for-primes min max)
  (cond
    ((even? min) (search-for-primes (+ min 1) max))  ; start with an odd minimum number
    ((not (> min max)) (begin
                  (timed-prime-test min)
                  (search-for-primes (+ min 2) max)))))

(search-for-primes 1000 1019)
(newline)
(search-for-primes 10000 10037)
(newline)
(search-for-primes 100000 100043)
(newline)
(search-for-primes 1000000 1000037)
(newline)
(search-for-primes 1000000000 1000000021)

;; Answers

; I'd expect the time needed for computation to be approximately equal,
; because the Fermat test on a certain level of abstraction does not do more steps for higher numbers,
; than for lower numbers. The number of steps depends solely on the provided parameter `times`.

; The timings show, that this is probably correct.

; Discrepancies: Maybe there is a little bit more computation for higher numbers,
; because of higher number of steps required for basic operations like modulo and division.
; There could also be a performance loss,
; if the numbers are too high to be natively represented and need to be software emulated.