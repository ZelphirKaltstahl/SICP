#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define (equal? list1 list2)
  (cond
    [(and (pair? list1) (pair? list2))
      (and
        (equal? (car list1) (car list2))
        (equal? (cdr list1) (cdr list2)))]
    [else (eq? list1 list2)]))

(equal? '(this is a list) '(this (is a) list))
(equal? '(this is a list) '(this is a list))
