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

(define (union-ordered-set set1 set2)
  (define (iter result subset1 subset2)
    (cond
      [(and (empty? subset1) (empty? subset2))
        result]

      [(empty? subset1)
        (cond
          [(= (car subset2)(car result)) (iter result subset1 (cdr subset2))]
          [else (iter (cons (car subset2) result) subset1 (cdr subset2))])]  ; is always the > case, because smaller elements get added first!

      [(empty? subset2)
        (cond
          [(= (car subset1) (car result)) (iter result (car subset1) subset2)]
          [else (iter (cons (car subset1) result) (cdr subset1) subset2)])]  ; is always the > case
      [else  ; set1 and set2 are not empty
        (cond
          [(empty? result)  ; initial case
            (cond
              [(= (car subset1) (car subset2))
                (iter (cons (car subset1) result) (cdr subset1) (cdr subset2))]

              [(> (car subset1) (car subset2))
                (iter (cons (car subset2) result) subset1 (cdr subset2))]

              [else  ; set1_1 < set2_1
                (iter (cons (car subset1) result) (cdr subset1) subset2)])]

          [(> (car subset1) (car subset2))
            (iter (cons (car subset2) result) subset1 (cdr subset2))]

          [(< (car subset1) (car subset2))
            (iter (cons (car subset1) result) (cdr subset1) subset2)]

          [else  ; both first elements are equal
            (cond
              [(> (car subset1) (car result))
                (iter (cons (car subset1) result) (cdr subset1) (cdr subset2))]
              [else
                (iter result (cdr subset1) (cdr subset2))])])]))
  (reverse(iter nil set1 set2)))


