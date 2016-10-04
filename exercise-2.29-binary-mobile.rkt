#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.29

;; predefined code
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

;; a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b.
(define (weight? part)
  (number? part))

(define (structure? part)
  (or
    (weight? part)
    (pair? part)))

(define (mobile? part)
  (and
    (pair? (car part))
    (pair? (cadr part))))

(define (branch-weight branch)
  (if
    (and (weight? (car branch)) (weight? (cadr branch)))  ; weight weight case
    (+ (car branch) (cadr branch))
    (+ (car branch) (total-weight (cadr branch)))  ; weight mobile case
  ))

(define (total-weight mobile)
  (+
    (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))))

;; c.
;; d.

;; UNIT TESTS
(define (check-equal?-with-output a b failure-msg)
  (display "checking for equality:") (newline)
  (display a) (newline)
  (display b) (newline)
  (check-equal? a b failure-msg))

(define (run-test-newlines a-test-suite)
  (for-each
    (λ (elem)
      (display elem) (newline))
    (run-test a-test-suite)))

(define exercise-test
  (test-suite
    "exercise 2.22 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    (test-case
      "does left-branch return the left branch?"
      (check-equal?
        (left-branch (make-mobile
          (make-branch
            1
            (make-mobile
              (make-branch 2 3)
              (make-branch 4 5)))
          (make-branch
            6
            (make-mobile
              (make-branch 7 8)
              (make-branch 9 10)))))
        (make-branch
          1
          (make-mobile
            (make-branch 2 3)
            (make-branch 4 5)))
        "left-branch does not return the left branch"))

    (test-case
      "does right-branch return the right branch?"
      (check-equal?
        (right-branch (make-mobile
          (make-branch
            1
            (make-mobile
              (make-branch 2 3)
              (make-branch 4 5)))
          (make-branch
            6
            (make-mobile
              (make-branch 7 8)
              (make-branch 9 10)))))
        (make-branch
            6
            (make-mobile
              (make-branch 7 8)
              (make-branch 9 10)))
        "right-branch does not return the right branch"))

    (test-case
      "does branch-length return the length of the branch?"
      (check-equal?
        (branch-length (make-branch
          1
          (make-mobile
            (make-branch 2 3)
            (make-branch 4 5))))
        1
        "branch-length does not return the length of the branch"))

    (test-case
      "does branch-structure return the (sub)structure of the branch?"
      (check-equal?
        (branch-structure (make-branch
          1
          (make-mobile
            (make-branch 2 3)
            (make-branch 4 5))))
        (make-mobile
            (make-branch 2 3)
            (make-branch 4 5))
        "branch-structure does not return the (sub)structure of the branch"))

    (test-case
      "does weight? return true for weights?"
      (check-true
        (weight? (branch-structure (left-branch (branch-structure (make-branch
          1
          (make-mobile
            (make-branch 2 3)
            (make-branch 4 5)))))))
        "weight? does not return true for weights"))

    (test-case
      "does structure? return #t for structures?"
      (check-true
        (structure? (branch-structure (left-branch (branch-structure
          (make-branch
            1
            (make-mobile
              (make-branch 2 3)
              (make-branch 4 5)))))))
        "structure? does not return #t for structures")
      (check-true
        (structure? (left-branch (branch-structure
          (make-branch
            1
            (make-mobile
              (make-branch 2 3)
              (make-branch 4 5))))))
        "structure? does not return #t for structures"))

    (test-case
      "does the mobile? predicate return #t for mobiles?"
      (check-true
        (mobile? (branch-structure
          (make-branch
            1
            (make-mobile
              (make-branch 2 3)
              (make-branch 4 5)))))
        "the mobile? predicate does not return #t for mobiles"))

    (test-case
      "does the mobile? predicate return #f for non-mobiles?"
      (check-false
        (mobile? (left-branch (branch-structure
          (make-branch
            1
            (make-mobile
              (make-branch 2 3)
              (make-branch 4 5))))))
        "the mobile? predicate does not return #f non-mobiles"))

    (test-case
      "does branch-weight return the correct branch weight?"
      (check-equal?
        (branch-weight (left-branch
          (make-mobile
            (make-branch
              1
              (make-mobile
                (make-branch 2 3)
                (make-branch 4 5)))
            (make-branch
              6
              (make-mobile
                (make-branch 7 8)
                (make-branch 9 10))))))
        15
        "branch-weight does not return the correct branch weight")
      (check-equal?
        (branch-weight (right-branch
          (make-mobile
            (make-branch
              1
              (make-mobile
                (make-branch 2 3)
                (make-branch 4 5)))
            (make-branch
              6
              (make-mobile
                (make-branch 7 8)
                (make-branch 9 10))))))
        (+ 6 7 8 9 10)
        "branch-weight does not return the correct branch weight"))
    (test-case
      "does total-weight return the correct weight for the mobile?"
      (check-equal?
        (total-weight
          (make-mobile
              (make-branch
                1
                (make-mobile
                  (make-branch 2 3)
                  (make-branch 4 5)))
              (make-branch
                6
                (make-mobile
                  (make-branch 7 8)
                  (make-branch 9 10)))))
        (+ 1 2 3 4 5 6 7 8 9 10)
        "total-weight does not return the correct weight for the mobile"))
  ))

(run-test-newlines exercise-test)

