#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

(provide (all-defined-out))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-binary-tree? x myset)
  (cond
    [(empty? myset) (false)]
    [(= x (entry myset)) true]
    [(< x (entry myset)) (element-of-set-binary-tree? x (left-branch myset))]
    [(> x (entry myset)) (element-of-set-binary-tree? x (right-branch myset))]))

(define (adjoin-set x myset)
  (cond
    [(empty? myset) (make-tree x nil nil)]
    [(= x (entry myset)) myset]
    [(< x (entry myset))
      (make-tree
        (entry myset)
        (adjoin-set x (left-branch myset))
        (right-branch myset))]
    [(> x (entry myset))
      (make-tree
        (entry myset)
        (left-branch myset)
        (adjoin-set x (right-branch myset)))]))


