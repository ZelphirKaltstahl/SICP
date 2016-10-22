#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define a 1)
(define b 2)

(list a b)

(list 'a 'b)

(list 'a b)

(list a 'b)

; (quote ...) is the same as '...
(car (quote (a b c)))
(cdr (quote (a b c)))


