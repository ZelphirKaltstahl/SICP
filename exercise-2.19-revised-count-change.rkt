#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define nil '())

; (cc 100 us-coins)
; 292

(define (cc amount coin-values)
  (cond
    [(= amount 0) 1]
    [(or (< amount 0) (no-more? coin-values)) 0]
    [else
      (+
        (cc
          amount
          (except-first-denomination coin-values))
        (cc
          (- amount (first-denomination coin-values))
          coin-values))]))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define us-coins2 (list 25 10 5 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define uk-coins2 (list 50 20 10 5 2 1 0.5 100))

(cc 100 us-coins)
(cc 100 uk-coins)
(cc 100 us-coins2)
(cc 100 uk-coins2)

; 292
; 104561
; 292
; 104561

; The result does not change.
; In the branch, where we use all kinds of coins, the order is not important
; In the branch, where we use all except the first kind of coin, we do not eliminate any changes prematurely, because all possible changes for the amount without the removed coin are calculated in the other branch.

