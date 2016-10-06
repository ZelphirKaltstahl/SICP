#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; predefined code
(define (scale-tree tree factor)
  (cond
    [(empty? tree) nil]
    [(not (pair? tree)) (* tree factor)]
    [else (cons
      (scale-tree (car tree) factor)
      (scale-tree (cdr tree) factor))]))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree-map tree factor)
  (map
    (lambda (subtree)
      (if
        (pair? subtree)
        (scale-tree-map subtree factor)
        (* subtree factor)))
    tree))

(scale-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; EXERCISE 2.30
(define (square-tree tree)
  (cond
    [(empty? tree) nil]
    [(not (pair? tree)) (* tree tree)]
    [else (cons
      (square-tree (car tree))
      (square-tree (cdr tree)))]))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-map tree)
  (map
    (lambda (subtree)
      (if
        (pair? subtree)
        (square-tree-map subtree)
        (* subtree subtree)))
    tree))

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))

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
    "exercise 2.30 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    
  ))

(time (run-test-newlines exercise-test))
