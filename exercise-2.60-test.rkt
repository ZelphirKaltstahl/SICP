#lang racket

(require rackunit
         "exercise-2.60-duplicate-set.rkt")

;;; TEST CASES
(test-case "element-of-duplicate-set? test case"
           (check-equal? (element-of-duplicate-set? 1 '(2 3 1))
                         true
                         "element-of-duplicate-set? does not work correctly.")
           (check-equal? (element-of-duplicate-set? 4 '(2 3 1))
                         false
                         "element-of-duplicate-set? does not work correctly.")
           (check-equal? (element-of-duplicate-set? '(1) '(2 3 1))
                         false
                         "element-of-duplicate-set? does not work correctly.")
           (check-equal? (element-of-duplicate-set? 'a '())
                         false
                         "element-of-duplicate-set? does not work correctly."))

(test-case "adjoin-duplicate-set test case"
           (check-equal? (adjoin-duplicate-set 'a '(1 2 3))
                         '(a 1 2 3)
                         "adjoin-duplicate-set does not work correctly.")
           (check-equal? (adjoin-duplicate-set '() '(1 2 3))
                         '(() 1 2 3)
                         "adjoin-duplicate-set does not work correctly.")
           (check-equal? (adjoin-duplicate-set '(1 2) '(1 2 3))
                         '((1 2) 1 2 3)
                         "adjoin-duplicate-set does not work correctly.")
           (check-equal? (adjoin-duplicate-set '(3) '(1 2 3 (3)))
                         '((3) 1 2 3 (3))
                         "adjoin-duplicate-set does not work correctly.")
           (check-equal? (adjoin-duplicate-set 3 '(1 2 3 (3)))
                         '(3 1 2 3 (3))
                         "adjoin-duplicate-set does not work correctly.")
           (check-equal? (adjoin-duplicate-set 3 '(1 2 (3)))
                         '(3 1 2 (3))
                         "adjoin-duplicate-set does not work correctly."))

(test-case "intersection-duplicate-set test case"
           (check-equal? (intersection-duplicate-set '() '(4 5 6))
                         '()
                         "intersection-duplicate-set does not work correctly.")
           (check-equal? (intersection-duplicate-set '(1 2 3) '())
                         '()
                         "intersection-duplicate-set does not work correctly.")
           (check-equal? (intersection-duplicate-set '(1 2 3) '(1 2 6))
                         '(1 2 1 2)
                         "intersection-duplicate-set does not work correctly.")
           (check-equal? (intersection-duplicate-set '(1 2 3) '(1 5 6))
                         '(1 1)
                         "intersection-duplicate-set does not work correctly.")
           (check-equal? (intersection-duplicate-set '(1 2) '(1 2 2 3))
                         '(1 2 1 2 2)
                         "intersection-duplicate-set does not work correctly.")
           (check-equal? (intersection-duplicate-set '(0 1 1 2 2 3) '(2 2 3 4))
                         '(2 2 3 2 2 3)
                         "intersection-duplicate-set does not work correctly."))

(test-case "union-duplicate-set test case"
   (check-true
     (let
       [(result (union-duplicate-set '(1 2 3) '(4 5 6)))]
       [and (element-of-duplicate-set? 1 result)
            (element-of-duplicate-set? 2 result)
            (element-of-duplicate-set? 3 result)
            (element-of-duplicate-set? 4 result)
            (element-of-duplicate-set? 5 result)
            (element-of-duplicate-set? 6 result)])
     "union-duplicate-set does not work correctly.")
   (check-equal? (union-duplicate-set '(1 2 3) '(1 2 6))
                 '(1 2 3 1 2 6)
                 "union-duplicate-set does not work correctly.")
   (check-true
     (let
       [(result (union-duplicate-set '(1 2 3) '(1 2 6)))]
       [and (element-of-duplicate-set? 1 result)
            (element-of-duplicate-set? 2 result)
            (element-of-duplicate-set? 3 result)
            (element-of-duplicate-set? 6 result)])
     "union-duplicate-set does not work correctly.")
   (check-true
     (let
       [(result (union-duplicate-set '() '(4 5 6)))]
       [and (element-of-duplicate-set? 4 result)
            (element-of-duplicate-set? 5 result)
            (element-of-duplicate-set? 6 result)])
     "union-duplicate-set does not work correctly.")
   (check-true
     (let
       [(result (union-duplicate-set '(1 2 3) '()))]
       [and (element-of-duplicate-set? 1 result)
            (element-of-duplicate-set? 2 result)
            (element-of-duplicate-set? 3 result)])
     "union-duplicate-set does not work correctly."))
