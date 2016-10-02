#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define one-through-four (list 1 2 3 4 5))

(define (last-pair mylist)
  (if
    (null? (cdr mylist))
    mylist
    (last-pair (cdr mylist))))

(last-pair one-through-four)
