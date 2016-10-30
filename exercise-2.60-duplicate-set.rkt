#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

(provide (all-defined-out))

(define (element-of-duplicate-set? x set)
  (cond [(empty? set) false]
        [(equal? x (car set)) true]
        [else (element-of-duplicate-set? x (cdr set))]))

(define (adjoin-duplicate-set x set)
  (cons x set))

(define (intersection-duplicate-set set1 set2)
  (define (iter result s1 s2 already-swapped)
    (display "intersection of:") (newline)
    (display "result: ") (display result) (newline)
    (display "s1: ") (display s1) (newline)
    (display "s2: ") (display s2) (newline)

    (cond
      [(and (empty? s1) (empty? s2))
        result]
      
      [(and (empty? s1) (not already-swapped))
        (iter result s2 result true)]

      [(and (empty? s1) already-swapped)
        result]

      [(element-of-duplicate-set? (car s1) s2)
        (iter (append result (list (car s1)))
              (cdr s1)
              s2
              already-swapped)]

      [else
        (iter result (cdr s1) s2 already-swapped)]))

  (iter '() set1 set2 false))

(define (union-duplicate-set set1 set2)
  (append set1 set2))
