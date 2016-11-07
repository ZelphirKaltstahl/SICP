#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

(provide (all-defined-out))

(define (element-of-ordered-set? x my-set)
  (cond
    [(empty? my-set) false]
    [(= x (car my-set)) true]
    [(< x (car my-set)) false]
    [else (element-of-ordered-set? x (cdr my-set))]))

(define (intersection-ordered-set set1 set2)
  (if
    (or (empty? set1) (empty? set2))
    '()
    (let
      [(x1 (car set1))
      (x2 (car set2))]
      [cond
        [(= x1 x2)
          (cons x1 (intersection-ordered-set (cdr set1) (cdr set2)))]  ; build the list using cons and recursive call
        [(< x1 x2)
          (intersection-ordered-set (cdr set1) set2)]
        [else
          (intersection-ordered-set set1 (cdr set2))]])))

(define (adjoin-ordered-set elem a-set)
  (cond
    ; or shortcuts, so no need to test for (empty? (car a-set)) a second time
    [(or (empty? a-set) (< elem (car a-set))) (cons elem a-set)]
    [(= elem (car a-set)) a-set]
    [else (cons (car a-set) (adjoin-ordered-set elem (cdr a-set)))]))


