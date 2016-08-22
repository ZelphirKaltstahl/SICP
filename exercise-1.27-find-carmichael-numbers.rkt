#lang racket
;; needed for compatibility reasons
(#%require (only racket/base current-inexact-milliseconds))
(define (runtime) (current-inexact-milliseconds))


;; basic function
(define (halve n) (/ n 2))
(define (decrement n) (- n 1))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? n) (divides? 2 n))

(define (next n)
  (if
   (= n 2)
   3
   (+ n 2)))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder
                   (square (expmod base (halve exp) m))  ; here is a trick: The squaring happens outside!
                   m))
    (else (remainder
            (* base (expmod base (decrement exp) m))
            m))))

;; exact prime number test
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))


;; for 1.27 specifically
(define (fermat-test number-to-check a)
  ;; performs fermat test for an a < n
  (= (expmod a number-to-check number-to-check) a))

(define (check-fermat-all n counter)
  ;; performs fermat test for all a < n
  ;; (actual answer for the exercise)
  (if
   (< counter n)
   (if
     (fermat-test n counter)
     (check-fermat-all n (+ counter 1))
     false)
   true))

(define (find-carmichael-numbers min max)
  ;; finds all Carmichael numbers within given limits min and max
  (if
   (< min max)
   (cond
     ((and (check-fermat-all min 0) (not (prime? min))) (begin
                                                          (display min)
                                                          (newline)
                                                          (find-carmichael-numbers (+ min 1) max)))
     (else (find-carmichael-numbers (+ min 1) max)))
   (display "finished")))

(find-carmichael-numbers 0 100000)

; 561
; 1105
; 1729
; 2465
; 2821
; 6601
; 8911
; 10585
; 15841
; 29341
; 41041
; 46657
; 52633
; 62745
; 63973
; 75361