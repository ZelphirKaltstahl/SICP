#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; EXERCISE 2.28
; hack solution, not efficient, not very clever
(define (fringe tree)
  (display "fringe called with: ") (display tree) (newline)
  (if
    (empty? tree)
    nil
    (if
      (pair? tree)
      (append (fringe (car tree)) (fringe (cdr tree)))
      (begin
        (display tree) (display " not empty and no pair!") (newline)
        (list tree)))))

; efficient solution
; this solution builds the list from the right most leaf to the left most leaf,
; which enables cons to create a list and thus makes the use of append and list
; unnecessary, which saves computational budget
(define (fringe2 tree)
  (define (not-pair? elem)
    (not (pair? elem)))
  (define (recur subtree result)
    (cond
      [(empty? subtree) result]
      [(not-pair? subtree) (cons subtree result)]
      [else (recur
        ; as a new subtree (left branch), which will be processed in the recur function at some point
        (car subtree)
        ; the result is the fringe of the right subtree, this way cons will to the correct job, putting further right leaves deeper nested than left leaves ones
        ; in order to get the first result, the recursion needs to reach the right most leaf.
        ; if the recursion is not at the right-most leaf yet, meaning the current subtree is a pair
        ; it will do another recursion step first, because it gets into this branch and the result
        ; is another recursion step.
        (recur (cdr subtree) result))]))
  (recur tree nil))

; a short solution using number?
; http://community.schemewiki.org/?sicp-ex-2.28
(define (fringe3 x)
  (cond
    ((null? x) x) 
    ((number? x) (list x)) 
    (else (append
      (fringe (car x))
      (fringe (cdr x))))))

;; UNIT TESTS
(define (check-equal?-with-output a b failure-msg)
  (display "checking for equality:") (newline)
  (display a) (newline)
  (display b) (newline)
  (check-equal? a b failure-msg))

(define exercise-test
  (test-suite
    "exercise 2.22 test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))

    (test-case
      "test if fringe works as it should"
      (check-equal?-with-output
        (fringe (list (list 1 2) (list 3 4)))
        (list 1 2 3 4)
        "fringe did not return the fringe"))
    (test-case
      "test if fringe works as it should"
      (check-equal?-with-output
        (fringe (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))
        (list 1 2 3 4 1 2 3 4)
        "fringe did not return the fringe"))
    (test-case
      "test if fringe works as it should"
      (check-equal?-with-output
        (fringe2 (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))
        (list 1 2 3 4 1 2 3 4)
        "fringe did not return the fringe"))
    (test-case
      "test if fringe works as it should"
      (check-equal?-with-output
        (fringe3 (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))
        (list 1 2 3 4 1 2 3 4)
        "fringe did not return the fringe"))
  ))

(run-test exercise-test)
