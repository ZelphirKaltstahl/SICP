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

;; EXERCISE 2.38
;  fold-right and fold-left

;; predefined code
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if
      (empty? rest)
      result
      (iter
        (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; guess: (((1 / 1) / 2) / 3) = 1/6 wrong
; correct is: (1 / (2 / (3 / 1))) = 3/2

(fold-left / 1 (list 1 2 3))
; guess: (1 / (1 / (2 / 3))) = 2/3 wrong
; correct is:(((1 / 1) / 2) / 3) = 1/6

(fold-right list nil (list 1 2 3))
; guess: (list 1 (list 2 (list 3 '()))) = (1 2 3)

(fold-left list nil (list 1 2 3))
; guess: (list (list (list () 1) 2) 3) = (((() 1) 2) 3)

; operation needs to be commutative to yield the same result

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
    
    
  ))

(run-test-newlines exercise-test)
