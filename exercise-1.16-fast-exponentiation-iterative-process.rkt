#lang racket

;; prerequisites
(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

;; slow recursive
(define (slow-expt-rec b n)
  (cond
    ((= n 0) 1)
    (else (* b (slow-expt-rec b (- n 1))))))

;; slow iterative
(define (slow-expt-iter b n)
  (slow-expt-iteration b n 1))

(define (slow-expt-iteration b n product)
  (cond
    ((= n 0) product)
    (else (slow-expt-iteration b (- n 1) (* b product)))))

;; fast recursive
(define (fast-expt-rec b n)
  (cond
    ;; neutral element of multiplication is 1
    ((= n 0) 1)
    ;; if n is even, simply square the basis to the power of n / 2, because it is the same
    ((even? n) (square (fast-expt-rec b (/ n 2))))
    ;; if n is not even, calculate b * b^(n-1), where (n-1) will be even
    (else (* b (fast-expt-rec b (- n 1))))))

;; fast iterative
(define (fast-expt-iter base exponent)
  (fast-expt-iteration base exponent 1))

(define (fast-expt-iteration base exponent product)
  (cond
    ((= exponent 0) product)
    ((even? exponent) (fast-expt-iteration (square base) (/ exponent 2) product))
    (else (fast-expt-iteration base (- exponent 1) (* product base)))))


;(time (slow-expt-iter 7 100000))  ; 4869
;(time (slow-expt-rec 7 100000))  ; 9748
;(time (fast-expt-rec 7 100000))  ; 4
(time (fast-expt-iter 7 100000))  ; 5