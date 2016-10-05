#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.32

(define (subsets set)
  (if
    (empty? set)
    (list nil)
    (let
      ; let rest be all subsets of the set except the first element
      [(rest (subsets (cdr set)))]
      [append
        ; all subsets of the set, which contains all elements except one
        rest
        ; for all set elements in (1)
        (map
          ; add the one element, which has been excluded before (this is like the coin change problem, but in reverse, adding that element again, instead of excluding more and more elements)
          (lambda (one-subset) (cons (car set) one-subset))
          ; (1) the set of all subsets of the set, which contains all elements but one
          rest)])))

;; Explanation:
; The idea behind this is, that the set of all subset is the set, which contains
; (1) all subsets without one of the elements of the original set (which reduces the problem size and will yield some result when only the empty list is left)
; (2) all subsets with the one elements of the original set, which is missing from the other subsets of (1)
; In order to get (1), we can simply do a recursive call.
; In order to get (2), we need to add the missing element to the result of (1), which we can do by using a let expression to temporarily store the result of (1) in a variable as done in the code.

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
    "exercise test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    (test-case
      "does the subsets procedure return all subsets?"
      (check-equal?
        (subsets (list 1 2 3))
        (list
          nil
          (list 3)
          (list 2)
          (list 2 3)
          (list 1)
          (list 1 3)
          (list 1 2)
          (list 1 2 3))
        "the subsets procedure does not return all subsets"))
  ))

(time (run-test-newlines exercise-test))
