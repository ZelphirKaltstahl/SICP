#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

(provide (all-defined-out))

(define (element-of-set? x set)
  (cond [(empty? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond [(or (empty? set1) (empty? set2)) '()]
        [(element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

(define (union-set set1 set2)
  (cond [(empty? set1) set2]
        [(empty? set2) set1]
        [(element-of-set? (car set1) set2)
          (union-set (cdr set1) set2)]
        [else
          (union-set (cdr set1)
                     (cons (car set1) set2))]))
