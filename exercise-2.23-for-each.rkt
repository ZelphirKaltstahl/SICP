#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.23

(define (for-each func items)
  (define (iter sublist)
    (if
      (empty? sublist)
      #t
      (begin
        (func (car sublist))
        (iter (cdr sublist)))))
  (iter items))

(for-each
  (lambda (x)
    (display (* x x))
    (newline))
  (list 1 2 3 4))

;; UNIT TESTS
(define exercise-test
  (test-suite
    "exercise 2.23 test"
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






