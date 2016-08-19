#lang racket
(define (avg a b)
  (/ (+ a b) 2))

(define (improve-guess x guess)
  (avg (/ x guess) guess))

(define (precision-sufficient x guess precision)
  (< (abs (- (* guess guess) x)) precision))

(define (new-if predicate then-clause else-clause)
  (cond
    (predicate then-clause)
    (else else-clause)))

(define (sqrt-iter guess x precision)
  (new-if
   (precision-sufficient x guess precision)
   guess
   (sqrt-iter (improve-guess x guess) x precision)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(sqrt-iter 3 10 0.001)

