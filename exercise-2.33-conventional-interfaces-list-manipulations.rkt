#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

; existing code from previous exercises
(define (odd? num)
  (= (remainder num 2) 1))

(define (even? num)
  (= (remainder num 2) 0))

(define (square x) (* x x))

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
  (display "got called with initial: ") (display initial) (newline)
  (display "got called with sequence: ") (display sequence) (newline)
  (if
    (empty? sequence)
    initial
    (op
      ; for append:
      ; given (1 2 3)
      ; cons 1 with the result of the call with (2 3)
      ; cons 2 with the result of the call with (3)
      ; cons 3 with the result of the call with nil ==> (1 (2 (3 (4 5 6)))) ==> appended!

      ; for length:
      ; 1 + result of the call with the rest of the sequence (y)
      ; 1 + result of the call with the rest of the sequence (y)
      ; 1 + result of the call with the rest of the sequence (y)
      ; ...
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

;; EXERCISE 2.33
;  - implementing common list manipulations in terms of conventional interfaces

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate
    (lambda (x y) (+ 1 y))
    0
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
      "test case for map"
      (check-equal?
        (map square (list 1 2 3 4 5))
        (list 1 4 9 16 25)
        "map does not work correctly"))
    
    (test-case
      "test case for append"
      (check-equal?
        (append (list 1 2 3) (list 4 5 6))
        (list 1 2 3 4 5 6)
        "append does not work correctly"))
    
    (test-case
      "test case for length"
      (check-equal?
        (length (list 1 2 3 4 5 6))
        6
        "length does not work correctly"))
  ))

(run-test exercise-test)
