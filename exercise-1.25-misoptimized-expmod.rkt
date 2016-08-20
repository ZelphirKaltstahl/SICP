#lang racket
;; needed for compatibility reasons
(#%require (only racket/base current-inexact-milliseconds))
(define (runtime) (current-inexact-milliseconds))


;; basic function
(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? n) (divides? 2 n))

(define (fast-expt-iter base exponent)
  (fast-expt-iteration base exponent 1))

(define (fast-expt-iteration base exponent product)
  (cond
    ((= exponent 0) product)
    ((even? exponent) (fast-expt-iteration (square base) (/ exponent 2) product))
    (else (fast-expt-iteration base (- exponent 1) (* product base)))))

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
                   (square (expmod base (halve exp) m))  ; here is a trick: The squaring happens outside!
                   m))
    (else (remainder
            (* base (expmod base (decrement exp) m))
            m))))

;(define (expmod base exp m)
;  (remainder (fast-expt-iter base exp) m))

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

; Alyssa is not correct. The code Alyssa suggested runs much slower than the original code.
; The expmod function uses a mathematical trick to keep calculating with low numbers.
; Instead of squaring the parameter for a recursive call to expmod, it squares the result of that call and then calculates the modulo operation.
; See the source code to find the line, where that happens.