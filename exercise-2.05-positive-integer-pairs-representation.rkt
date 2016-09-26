#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

; Representation of pairs as: 2^a * 3^b

; premiss 1:
; 2 and 3 are prime numbers.

; premiss 2:
; Every natural number has a prime factorization,
; which is unique in its substance, but not order of factors.

; premiss 3:
; However, the order is not important,
; as we only care about the exponents,
; which are the counts of the 2 factors and the 3 factors

; conclusion:
; To figure out, what a and b were,
; we simply can divide the integer by 2 and 3,
; if there is no remainder and count how many
; times we divided the integer by 2 and 3 until
; the result is 1. Those counts will be a and b.

; conclusion:
; We can also only divide by one of the numbers,
; until division by that number would leave a remainder,
; in order to find only one of the numbers of the pair
; of integers.

(define (cons a b)
  (*
    (expt 2 a)
    (expt 3 b)))

(define (car number)
  (define (iter factor-count current-number)
    (if
      (= (remainder current-number 2) 0)
      (iter (+ factor-count 1) (/ current-number 2))
      factor-count))
  (iter 0 number))

(define (cdr number)
  (define (iter factor-count current-number)
    (if
      (= (remainder current-number 3) 0)
      (iter (+ factor-count 1) (/ current-number 3))
      factor-count))
  (iter 0 number))

(time (cons 11 20))
(time (car (cons 11 20)))
(time (cdr (cons 11 20)))

(time (cons 123 320))
(time (car (cons 123 320)))
(time (cdr (cons 123 320)))

(time (cons 13135 320))
(time (car (cons 13135 320)))
(time (cdr (cons 13135 320)))
