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

(define (fib n)
  (define (fib-iter a b p q count)
    (cond
      ((= count 0) b)
      ((even? count) (fib-iter
                      a
                      b
                      (+ (* p p) (* q q))
                      (+ (* 2 p q) (* q q))
                      (/ count 2)))
      (else (fib-iter
             (+ (* b q) (* a q) (* a p))
             (+ (* b p) (* a q))
             p
             q
             (- count 1)))))
  (fib-iter 1 0 0 1 n))

; new code
(define (filter predicate sequence)
  (cond
    [(null? sequence)
      nil]
    [(predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))]
    [else
      (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
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

; now we are ready to use these procedures
(define (sum-odd-squares tree)
  (accumulate
    +
    0
    (map
      square
      (filter
        odd?
        (enumerate-tree tree)))))

(define (even-fibs maximum)
  (accumulate
    cons
    nil
    (filter
      even?
      (map fib (enumerate-interval 0 maximum)))))

(define (list-fib-squares maximum)
  (accumulate
    cons
    nil
    (map
      square
      (map
        fib
        (enumerate-interval 0 maximum)))))

(define (prod-of-squares-of-odd-elems sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

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
      "does filter work correctly?"
      (check-equal?
        (filter odd? (list 1 2 3 4 5 6 7))
        (list 1 3 5 7)
        "filter does not work correctly")
      (check-equal?
        (filter even? (list 1 2 3 4 5 6 7))
        (list 2 4 6)
        "filter does not work correctly"))
    
    (test-case
      "does accumulate work correctly?"
      (check-equal?
        (accumulate + 0 (list 1 2 3 4 5 6 7 8 9 10))
        55
        "accumulate does not work correctly")
      (check-equal?
        (accumulate * 1 (list 1 2 3 4 5))
        120
        "accumulate does not work correctly"))
    
    (test-case
      "does enumerate-interval work correctly?"
      (check-equal?
        (enumerate-interval 1 10)
        (list 1 2 3 4 5 6 7 8 9 10)
        "enumerate-interval does not work correctly")
      (check-equal?
        (enumerate-interval -10 1)
        (list -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1)
        "enumerate-interval does not work correctly"))
    
    (test-case
      "does enumerate-tree work correctly?"
      (check-equal?
        (enumerate-tree (list (list 4 3) (list (list 1 2) 5 (list 10 7))))
        (list 4 3 1 2 5 10 7)
        "enumerate-tree does not work correctly"))
    
    (test-case
      "does even-fibs only return even fibonacci numbers?"
      (check-equal?
        (even-fibs 12)
        (list 0 2 8 34 144)
        "even-fibs does not only return even fibonacci numbers"))
    
    (test-case
      "test case for list-fib-squares"
      (check-equal?
        (list-fib-squares 5)
        (list 0 1 1 4 9 25)
        "list-fib-squares does not work correctly"))
    
    (test-case
      "test case for prod-of-squares-of-odd-elems"
      (check-equal?
        (prod-of-squares-of-odd-elems (list 1 3 2 5 6 9 10 11))
        (* (* 1 1) (* 3 3) (* 5 5) (* 9 9) (* 11 11))
        "prod-of-squares-of-odd-elems does not work correctly"))
  ))

(time (run-test-newlines exercise-test))
