#lang racket

;; recursive
(define (* a b)
  ;; calculates the product of two numbers by successive addition
  (if
    (= b 0)
    0
    (+ a (* a (- b 1)))))


;; fast multiplication
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-multiply a b)
  (fast-mult a b 0))

(define (fast-mult a b product)
  ;(print (list a b product))
  (cond
    ((= b 0) product)
    ((= (remainder b 2) 0) (fast-mult (double a) (halve b) product))
    (else (fast-mult a (- b 1) (+ product a)))))


;; tests
(time (* 400000 500000))
(time (fast-multiply 400000 500000))