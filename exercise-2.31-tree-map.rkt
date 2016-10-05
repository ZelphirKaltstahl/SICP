#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.31
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (tree-map func tree)
  (map
    (lambda (subtree)
      (if
        (pair? subtree)
        (tree-map func subtree)
        (func subtree)))
    tree))

(define (square-tree tree) (tree-map square tree))
(define (cube-tree tree) (tree-map cube tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(cube-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

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

    
  ))

(time (run-test-newlines exercise-test))
