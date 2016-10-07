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

(define (accumulate-n op init seqs)
  (if
    (empty? (car seqs))
    nil
    (cons
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

;; EXERCISE 2.37
;  - accumulate columns

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector mat vect)
  (map
    (lambda (row) (dot-product row vect))
    mat))

(define (transpose mat)
  (accumulate-n
    (lambda (current next)
      (cons current next))
    nil
    mat))

(define (matrix-*-matrix m n)
  (let
    [(cols (transpose n))]
    [map
      (lambda (row)
        (matrix-*-vector cols row))
      m]))

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
      "test case for dot-product"
      (check-equal?
        (dot-product
          (list 1 2 3)
          (list 4 5 6))
        (+ (* 1 4) (* 2 5) (* 3 6))
        "dot-product does not work correctly"))
    (test-case
      "test case for matrix-*-vector"
      (check-equal?
        (matrix-*-vector
          (list
            (list 1 -1 2)
            (list 0 -3 1))
          (list 2 1 0))
        (list 1 -3)
        "matrix-*-vector does not work correctly"))
    (test-case
      "test case for transpose procedure"
      (check-equal?
        (transpose
          (list
            (list 1 2 3)
            (list 4 5 6)
            (list 7 8 9)))
        (list
          (list 1 4 7)
          (list 2 5 8)
          (list 3 6 9))
        "transpose does not work correctly"))
    (test-case
      "test case for matrix-*-matrix"
      (check-equal?
        (matrix-*-matrix
          (list
            (list 0 4 -2)
            (list -4 -3 0))
          (list
            (list 0 1)
            (list 1 -1)
            (list 2 3)))
        (list
          (list 0 -10)
          (list -3 -1))
        "matrix-*-matrix does not work correctly"))
  ))

(run-test-newlines exercise-test)
