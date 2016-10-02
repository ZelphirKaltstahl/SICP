#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.24

(define (count-leaves tree)
  (cond
    [(empty? tree) 0]
    [(not (pair? tree)) 1]
    [else (+
      (count-leaves (car tree))
      (count-leaves (cdr tree)))]))

(define atree (cons (list 1 2) (list 3 4)))
atree
(count-leaves atree)
(length atree)

(list 1 (list 2 (list 3 4)))


;; UNIT TESTS
(define exercise-test
  (test-suite
    "exercise 2.22 test"
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






