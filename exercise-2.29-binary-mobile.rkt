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

(define (len? part)
  (number? part))

(define (structure? part)
  (or
    (weight? part)
    (pair? part)))

(define (mobile? part)
  (and
    (pair? (car part))
    (pair? (cadr part))))

(define (branch? structure)
  (if
    (not (pair? structure))
    #f
    (and
      (len? (car structure))
      (or
        (weight? (cadr structure))
        (mobile? (cadr structure))))))

(define (branch-weight branch)
  (if
    (weight? (cadr branch))  ; weight case
    (branch-structure branch)  ; if the attached thing is only a weight, just take that weight as weight
    (total-weight (branch-structure branch))))  ; mobile case

(define (total-weight mobile)
  (+
    (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))))

;; c.
(define (torque branch)
  (* (branch-weight branch) (branch-length branch)))

(define (balanced? mobile)
  (define (traverse structure result)
    (cond
      ; weights in themselves cannot be balanced or unbalanced, so they should not influence the result of an and expression
      [(weight? structure) result]
      ; of branches there are two types, one with having a weight attached as cadr (1) and one with a submobile attached as cadr (2), so we distinguish the cases here
      [(branch? structure)
        (cond
          ; weights in themselves cannot be balanced or unbalanced, so they should not change the result alone
          [(weight? (cadr structure)) result]
          ; if a submobile is attached, do a recursive call to check the whole submobile
          [(mobile? (cadr structure)) (balanced? (cadr structure))])]
      ; a mobile is balanced, if:
      ; (1) the torques of the left and right branch are equal AND
      ; (2) each of the submobiles on branches of the mobile are also balanced
      [(mobile? structure)
        (and
          ; this is the only place where a #f can be created
          (=
            (torque (left-branch structure))
            (torque (right-branch structure)))
          ; here we have only recursive calls
          (traverse (left-branch structure) result)
          (traverse (right-branch structure) result))]))
  (traverse mobile #t))

;; d.
; if we changed the constructors for branches and mobiles, we would only have to change the selectors for parts of mobiles and other code, which selects parts of a mobile but does not use selectors, but uses car or cadr instead. one could improve upon that by creating procedures, which eliminate car and cadr from the procedures, except in the selectors. in that case one would only have to change those selectors. the selectors represent an abstraction barrier.

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
        (+ 3 5)
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
        (+ 8 10)
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
        (+ 3 5 8 10)
        "total-weight does not return the correct weight for the mobile"))

    (test-case
      "does the torque procedure calculate the torque correctly?"
      (check-equal?
        (torque (left-branch
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
        (* 1 (+ 3 5))
        "the torque procedure does not calculate the torque correctly")
      (check-equal?
        (torque (right-branch
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
        (* 6 (+ 8 10))
        "the torque procedure does not calculate the torque correctly"))

    (test-case
      "does the branch? predicate return #t for a branch?"
      (check-true
        (branch? (left-branch
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
        "the branch? predicate returns #f for a branch"))

    (test-case
      "does the branch? predicate return #f for a non-branch?"
      (check-false
        (branch? (branch-structure (left-branch
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
                (make-branch 9 10)))))))
        "the branch? predicate returns #t for a non-branch"))

    (test-case
      "does the predicate balanced? return #t for a balanced tree?"
      (check-true
        (balanced?
          (make-mobile
            (make-branch
              4
              (make-mobile
                (make-branch 2 6)
                (make-branch 3 4)))
            (make-branch
              8
              (make-mobile
                (make-branch 2 4)
                (make-branch 8 1)))))
        "the predicate balanced? wrongly returns #f for a balanced tree"))

    (test-case
      "does the predicate balanced? return #f for an unbalanced tree?"
      (check-false
        (balanced?
          (make-mobile
            (make-branch
              4
              (make-mobile
                (make-branch 2 6)
                (make-branch 3 4)))
            (make-branch
              8
              (make-mobile
                (make-branch 2 4)
                (make-branch 8 2)))))
        "the predicate balanced? wrongly returns #t for an unbalanced tree"))
  ))

(time (run-test-newlines exercise-test))
