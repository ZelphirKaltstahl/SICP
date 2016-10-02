#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define (scale-list items factor)
  (if
    (empty? items)
    nil
    (cons
      (* (car items) factor)
      (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if
    (empty? items)
    nil
    (cons
      (proc (car items))
      (map proc (cdr items)))))

(map
  abs
  (list -10 2.5 -11.6 17))

(map
  (λ (x) (* x x))
  (list 1 2 3 4))

(define (scale-list-with-map items factor)
  (map
    (λ (x) (* x factor))
    items))

(scale-list-with-map (list 1 2 3 4 5) 10)

;; EXERCISE 2.21

(define (square-list items)
  (if
    (empty? items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map
    (λ (x) (* x x))
    items))

(define exercise-test
  (test-suite
    "exercise 2.21 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    (test-case
      "check if both procedures yield the same result"
      (check-equal?
        (square-list (list 1 2 3 4))
        (square-list-map (list 1 2 3 4))
        "the two procedures do not yield the same result"))
    (test-case
      "check the result of one of the functions"
      (check-equal?
        (square-list (list 2 3 5))
        (list 4 9 25)
        "the procedure does not yield a correct result"))))

(run-test exercise-test)






