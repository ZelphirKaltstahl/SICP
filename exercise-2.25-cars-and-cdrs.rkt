#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.25

;; predefined sequences
(define a (list 1 3 (list 5 7) 9))
(define b (list (list 7)))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

a
b
c

;; getting the 7s
(car (cdr (car (cddr a))))
(caar b)
(cadr (cadr (cadr (cadr (cadr (cadr c))))))

;; UNIT TESTS
(define exercise-test
  (test-suite
    "exercise 2.25 test"
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






