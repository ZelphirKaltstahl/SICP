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

;; EXERCISE 2.36
;  - accumulate columns

(define (accumulate-n op init seqs)
  (if
    (empty? (car seqs))  ; if the first sequence is empty
    nil                  ; return the empty list
    (cons                ; else cons
      (accumulate op init (map car seqs))       ; accumulate all first elements of the sequences
      (accumulate-n op init (map cdr seqs)))))  ; recursive call for the rest of the lists

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
      "test case for accumulate-n procedure"
      (check-equal?
        (accumulate-n + 0 (list
          (list 4 2 10)
          (list 2 1 6)
          (list 5 4 5)))
        (list 11 7 21)
        "procedure accumulate-n does not work correctly")
      (check-equal?
        (accumulate-n * 1 (list
          (list 4 2 10)
          (list 2 1 6)
          (list 5 4 5)))
        (list 40 8 300)
        "procedure accumulate-n does not work correctly"))
  ))

(run-test exercise-test)
