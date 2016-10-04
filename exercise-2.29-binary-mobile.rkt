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
  (pair? part))

(define (mobile? part)
  (and
    (pair? (car part))
    (pair? (cadr part))))

(define (branch-weight branch)
  (if
    (and (weight? (car part)) (weight? (cadr part)))  ; weight weight case
    (+ (car part) (cadr part))
    (+ (car part) (total-weight (cadr part)))  ; weight mobile case
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

(define exercise-test
  (test-suite
    "exercise 2.22 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    (test-case
      "does left-branch return the left branch?"
      (check-equal?-with-output
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
              (make-branch 9 10))))
        (make-branch
          1
          (make-mobile
            (make-branch 2 3)
            (make-branch 4 5)))
        "left-branch does not return the left branch"))
  ))

(run-test exercise-test)
