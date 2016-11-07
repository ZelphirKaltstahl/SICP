#lang racket

(require rackunit
         "exercise-2.61-set-as-ordered-list.rkt")

;;; TEST CASES
(test-case "adjoin-ordered-set test case"
           (check-equal? (adjoin-ordered-set 1 '(2 3))
                         '(1 2 3)
                         "adjoin-ordered-set does not work correctly.")
           (check-equal? (adjoin-ordered-set 2 '(1 3))
                         '(1 2 3)
                         "adjoin-ordered-set does not work correctly.")
           (check-equal? (adjoin-ordered-set 3 '(1 2))
                         '(1 2 3)
                         "adjoin-ordered-set does not work correctly.")
           (check-equal? (adjoin-ordered-set 4 '(1 2))
                         '(1 2 4)
                         "adjoin-ordered-set does not work correctly."))


; (test-case "union-ordered-set test case"
;    (check-true
;      (let
;        [(result (union-ordered-set '(1 2 3) '(4 5 6)))]
;        [and (element-of-ordered-set? 1 result)
;             (element-of-ordered-set? 2 result)
;             (element-of-ordered-set? 3 result)
;             (element-of-ordered-set? 4 result)
;             (element-of-ordered-set? 5 result)
;             (element-of-ordered-set? 6 result)])
;      "union-ordered-set does not work correctly.")
;    (check-equal? (union-ordered-set '(1 2 3) '(1 2 6))
;                  '(1 2 3 1 2 6)
;                  "union-ordered-set does not work correctly.")
;    (check-true
;      (let
;        [(result (union-ordered-set '(1 2 3) '(1 2 6)))]
;        [and (element-of-ordered-set? 1 result)
;             (element-of-ordered-set? 2 result)
;             (element-of-ordered-set? 3 result)
;             (element-of-ordered-set? 6 result)])
;      "union-ordered-set does not work correctly.")
;    (check-true
;      (let
;        [(result (union-ordered-set '() '(4 5 6)))]
;        [and (element-of-ordered-set? 4 result)
;             (element-of-ordered-set? 5 result)
;             (element-of-ordered-set? 6 result)])
;      "union-ordered-set does not work correctly.")
;    (check-true
;      (let
;        [(result (union-ordered-set '(1 2 3) '()))]
;        [and (element-of-ordered-set? 1 result)
;             (element-of-ordered-set? 2 result)
;             (element-of-ordered-set? 3 result)])
;      "union-ordered-set does not work correctly."))
