#lang racket

;; recursive procedure
;; recursive process
(define (+ a b)
  (if
    (= a 0)
    b
    (inc (+ (dec a) b))))

;; recursive procedure
;; iterative process
(define (+ a b)
  (if
   (= a 0)
   b
   (+ (dec a) (inc b))))