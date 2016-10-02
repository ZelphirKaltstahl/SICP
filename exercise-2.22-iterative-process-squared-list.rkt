#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define (map proc items)
  (if
    (empty? items)
    nil
    (cons
      (proc (car items))
      (map proc (cdr items)))))

;; EXERCISE 2.22

;; Louis Reasoner's Code:
(define (square x)
  (* x x))

(define (square-list1 items)
  (define (iter things answer)
    (if
      (empty? things)
      answer
      (iter
        (cdr things)
        (cons (square (car things)) answer))))
  (iter items nil))

(define (square-list2 items)
  (define (iter things answer)
    (if
      (empty? things)
      answer
      (iter
        (cdr things)
        (cons answer (square (car things))))))
  (iter items nil))

(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))

; Observation:

; It is still not the same as the squared list, because the empty list is the first element and not the last element in the list. Furthermore the list structure is "wrapped" the wrong way around.

; Explanation:

; By adding another element using cons, the previous list is wrapped inside another pair. However, usually the rest of the list is wrapped with the current element, in order to get the proper list structure. The cons function takes the first argument as a given and does not change it. So if the first argument is already a list, it will not combine the two arguments into one list, but keep them separated.
; This behavior can be circumvented, if we use append instead. However it is more computationally heavy than cons.

(define (square-list3 items)
  (define (iter things answer)
    (if
      (empty? things)
      answer
      (iter
        (cdr things)
        (append answer (list (square (car things)))))))
  (iter items nil))

(square-list3 (list 1 2 3 4))

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






