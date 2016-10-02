#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...


(define (f x y . z)
  (cons x (cons y z)))

;; usage with lambda:
(define f2
  (λ (x y . z)
    (cons x (cons y z))))

(define g (λ w w))

(f 1 2 3 4 5 6)
(g 1 2 3 4 5)
