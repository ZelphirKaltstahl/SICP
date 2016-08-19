#lang racket
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-squares-two-max x y z)
  (cond
    ((= (min x y z) x) (sum-of-squares y z))
    ((= (min x y z) y) (sum-of-squares x z))
    (else (sum-of-squares x y))))

(sum-of-squares-two-max 10 20 14)