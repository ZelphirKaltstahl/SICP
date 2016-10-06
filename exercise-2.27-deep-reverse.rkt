#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; CODE FROM 2.18
(define (reverse items)
  (define (iter subitems result)
    (if
      (null? subitems)
      result
      (iter (cdr subitems) (cons (car subitems) result))))
  (iter items nil))

;; EXERCISE 2.27
(define (deep-reverse items)
  (define (iter subitems result)
    (if
      (null? subitems)
      result
      (iter (cdr subitems) (cons
        (if
          (pair? (car subitems))
          (deep-reverse (car subitems))
          (car subitems))
        result))))
  (iter items nil))

;; UNIT TESTS
(define (check-equal?-with-output a b failure-msg)
  (display "checking for equality:") (newline)
  (display a) (newline)
  (display b) (newline)
  (check-equal? a b failure-msg))

(define exercise-test
  (test-suite
    "exercise 2.27 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    (test-case
      "test if deep-reverse works as it should"
      (check-equal?-with-output
        (deep-reverse (list (list 1 2) (list 3 4)))
        (list (list 4 3) (list 2 1))
        "the list was not deep reversed"))
    (test-case
      "test if deep-reverse works as it should"
      (check-equal?-with-output
        (deep-reverse (list (list 1 2 (list 2.3 2.7)) (list 3 4)))
        (list (list 4 3) (list (list 2.7 2.3) 2 1))
        "the list was not deep reversed"))))

(run-test exercise-test)






