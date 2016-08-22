#lang racket

;; basic functions
(define (halve n) (/ n 2))
(define (decrement n) (- n 1))
(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (even? n) (divides? 2 n))
(define (next n)
  (if
   (even? n)
   (+ n 1)
   (+ n 2)))

;; prime number test
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))







(define (non-trivial-sqrt-of-one? base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder
                   (square (non-trivial-sqrt-of-one? base (halve exp) m))  ; here is a trick: The squaring happens outside!
                   m))
    (else (remainder
            (* base (non-trivial-sqrt-of-one? base (decrement exp) m))
            m))))


(define (rabin-miller-test number-to-check a)
  ; simply call the function with an (n-1) instead of n like with expmod previously
  (= (non-trivial-sqrt-of-one? a (- number-to-check 1) number-to-check) 1))

(define (check-rabin-miller-all n counter)
  (if
   (< counter n)
   (if (rabin-miller-test n counter)
     (check-rabin-miller-all n (+ counter 1))
     false)
   true))



(define (find-primes min max)
  ;; finds all prime numbers in between min and max (including min and excluding max)
  (if
   (< min max)
   (cond
     ((check-rabin-miller-all min 2) (begin
                                       (display min)
                                       (newline)
                                       (find-primes (+ min 1) max)))
     (else (find-primes (next min) max)))
   (display "finished")))

(find-primes 2 200)

; carmichael numbers
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

;; NOTE
;; For this exercise I didn't actually change the expmod function as described in the exercise.
;; I merely renamed it and changed the parameters given to it by the rabin-miller-test function to be the exponent - 1 and then check in the rabin-miller-test function for congruence to 1 (mod n).
;; This seems simpler to me than changing the expmod function and plays well with my other code.