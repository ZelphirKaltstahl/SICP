#lang racket

(require rackunit
         "exercise-2.59-union-set.rkt")

;;; TEST CASES
(test-case "element-of-set? test case"
           (check-equal? (element-of-set? 1 (list 2 3 1))
                         true
                         "element-of-set? does not work correctly.")
           (check-equal? (element-of-set? 4 (list 2 3 1))
                         false
                         "element-of-set? does not work correctly.")
           (check-equal? (element-of-set? (list 1) (list 2 3 1))
                         false
                         "element-of-set? does not work correctly.")
           (check-equal? (element-of-set? 'a '())
                         false
                         "element-of-set? does not work correctly."))

(test-case "adjoin-set test case"
           (check-equal? (adjoin-set 'a (list 1 2 3))
                         (list 'a 1 2 3)
                         "adjoin-set does not work correctly.")
           (check-equal? (adjoin-set '() (list 1 2 3))
                         (list '() 1 2 3)
                         "adjoin-set does not work correctly.")
           (check-equal? (adjoin-set (list 1 2) (list 1 2 3))
                         (list (list 1 2) 1 2 3)
                         "adjoin-set does not work correctly.")
           (check-equal? (adjoin-set (list 3) (list 1 2 3 (list 3)))
                         (list 1 2 3 (list 3))
                         "adjoin-set does not work correctly."))

(test-case "intersection-set test case"
           (check-equal? (intersection-set '() (list 4 5 6))
                         '()
                         "intersection-set does not work correctly.")
           (check-equal? (intersection-set (list 1 2 3) '())
                         '()
                         "intersection-set does not work correctly.")
           (check-equal? (intersection-set (list 1 2 3) (list 1 2 6))
                         (list 1 2)
                         "intersection-set does not work correctly.")
           (check-equal? (intersection-set (list 1 2 3) (list 1 5 6))
                         (list 1)
                         "intersection-set does not work correctly."))

(test-case "union-set test case"
   (check-true
     (let
       [(result (union-set (list 1 2 3) (list 4 5 6)))]
       [and (element-of-set? 1 result)
            (element-of-set? 2 result)
            (element-of-set? 3 result)
            (element-of-set? 4 result)
            (element-of-set? 5 result)
            (element-of-set? 6 result)])
     "union-set does not work correctly.")
   (check-true
     (let
       [(result (union-set (list 1 2 3) (list 1 2 6)))]
       [and (element-of-set? 1 result)
            (element-of-set? 2 result)
            (element-of-set? 3 result)
            (element-of-set? 6 result)])
     "union-set does not work correctly.")
   (check-true
     (let
       [(result (union-set '() (list 4 5 6)))]
       [and (element-of-set? 4 result)
            (element-of-set? 5 result)
            (element-of-set? 6 result)])
     "union-set does not work correctly.")
   (check-true
     (let
       [(result (union-set (list 1 2 3) '()))]
       [and (element-of-set? 1 result)
            (element-of-set? 2 result)
            (element-of-set? 3 result)])
     "union-set does not work correctly."))
