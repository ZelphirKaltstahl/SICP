#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define one-through-four (list 1 2 3 4 5))

one-through-four
(car one-through-four)
(cdr one-through-four)
(cadr one-through-four)
(cons 10 one-through-four)
(cons 5 one-through-four)
(cddddr one-through-four)
(cdr (cddddr one-through-four))

(define (mylength alist)
  (define (iter sublist count)
    (if (null? sublist)
      count
      (iter (cdr sublist) (+ count 1))))
  (iter alist 0))

(mylength one-through-four)
(length one-through-four)
