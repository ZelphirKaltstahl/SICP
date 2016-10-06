#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; PREDEFINED CODE
(define (filter predicate sequence)
  (cond
    [(null? sequence)
      nil]
    [(predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))]
    [else
      (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if
    (empty? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if
    (> low high)
    nil
    (cons
      low
      (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond
    [(empty? tree) nil]
    [(not (pair? tree)) (list tree)]
    [else (append
      (enumerate-tree (car tree))
      (enumerate-tree (cdr tree)))]))

;; EXERCISE 2.35
;  - implementation of count-leaves as an accumulation

(define (count-leaves-old tree)
  (cond
    [(empty? tree) 0]
    [(not (pair? tree)) 1]
    [else (+
      (count-leaves (car tree))
      (count-leaves (cdr tree)))]))

(define (count-leaves tree)
  (accumulate
    (λ (x y) (+ 1 y))
    0
    (map
      (λ (subtree)
        (cond
          [(not (pair? subtree)) 1]
          [else (+
            (count-leaves (car subtree))
            (count-leaves (cdr subtree)))]))
      tree)))

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
      "test for count-leaves procedure"
      (check-equal?-with-output
        (count-leaves
          (list
            1
            (list
              (list 2 3)
              (list 4 5))
            (list 6)))
        6
        "count-leaves does not work correctly"))
  ))

(run-test exercise-test)

(count-leaves
          (list
            1
            (list
              (list 2 3)
              (list 4 5))
            (list 6)))
