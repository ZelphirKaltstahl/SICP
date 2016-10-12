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
  ;; (1 2 3) --> (op 1 (op 2 (op 3 init)))
  ;; the sequence is folded from the right to the left,
  ;; the operation with the first element requires all previous results folded into the in between result
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

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  ;; (1 2 3) --> (op (op (op init 1) 2) 3)
  ;; the sequence is folded from the left to the right,
  ;; the operation with the last element requires all previous results folded into the in between result
  (define (iter result rest)
    (if
      (empty? rest)
      result
      (iter
        (op result (car rest))
        (cdr rest))))
  (iter initial sequence))


;; EXERCISE 2.39
;  reverse as fold operation

(define (reverse-fold-right sequence)
  (fold-right
    (lambda (x y) (cons y x))
    nil
    sequence))

(define (reverse-fold-left sequence)
  (fold-left
    (lambda (x y) (cons y x))
    nil
    sequence))

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
      "test case for reverse using fold right"
      (check-equal?-with-output
        (reverse-fold-right (list 1 2 3))
        (list 3 2 1)
        "the procedure reverse-fold-right does not work correctly"))
    
    (test-case
      "test case for reverse using fold left"
      (check-equal?-with-output
        (reverse-fold-left (list 1 2 3))
        (list 3 2 1)
        "the procedure reverse-fold-left does not work correctly"))
  ))

(run-test-newlines exercise-test)

;(reverse-fold-left (list 1 2 3))
;(reverse-fold-right (list 1 2 3))
