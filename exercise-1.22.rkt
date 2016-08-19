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
(search-for-primes 10000 10037)
(search-for-primes 100000 100019)
(search-for-primes 1000000 1000037)

; The timings reflect the square root 10 order of growth.

; Square root of 10 is approximately 3 and the timings get approximately 3 times larger.

; Higher numbers like 100000 and 1000000 still reflect this order of growth.

; The results are compatible with the notion,
; that programs run on my machine run in time proportional to
; the number of steps required for the computation.