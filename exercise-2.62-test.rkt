#lang racket

(require rackunit
         "exercise-2.62-union-ordered-set.rkt")

(test-case
  "union-ordered-set test case"
  (check-equal?
    (union-ordered-set '(1 2 4) '(3))
    '(1 2 3 4)
    "union-ordered-set does not work correctly")
  (check-equal?
    (union-ordered-set '(1 2) '(1 3))
    '(1 2 3)
    "union-ordered-set does not work correctly")
  (check-equal?
    (union-ordered-set '(2 4 9) '(1 4 9))
    '(1 2 4 9)
    "union-ordered-set does not work correctly"))
