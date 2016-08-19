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
(define (even? x) (= (remainder x 2) 0))

(define (fast-multiply a b)
  (fast-mult-rec a b))

(define (fast-mult-rec a b)
  ;(print (list a b product))
  (cond
    ((= b 0) 0)
    ((even? b) (fast-mult-rec (double a) (halve b)))
    (else (+ a (fast-mult-rec a (- b 1))))))


;; tests
(time (* 400000 500000))
(time (fast-multiply 400000 500000))