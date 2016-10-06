#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.26

;; predefined sequences
(define x (list 1 2 3))
(define y (list 4 5 6))

(display "guess for (append x y): (1 2 3 4 5 6)") (newline)
(append x y)
(display "guess for (cons x y): ((1 2 3) 4 5 6)") (newline)
(cons x y)
(display "guess for (list x y): ((1 2 3) (4 5 6))") (newline)
(list x y)

;; UNIT TESTS
(define exercise-test
  (test-suite
    "exercise 2.26 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

;    (test-case
;      "test case desc"
;      (check-equal?
;        (code)
;        code result
;        "failure desc"))
  ))

(run-test exercise-test)






