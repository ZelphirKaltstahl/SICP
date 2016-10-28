#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; GIVEN CODE
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and
    (variable? v1)
    (variable? v2)
    (eq? v1 v2)))

(define (=number? exp num)
  (and
    (number? exp)
    (= exp num)))

(define (make-sum a1 a2)
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and
      (number? a1)
      (number? a2))
      (+ a1 a2)]
    [(eq? a1 a2) (list '* 2 a1)]
    [else
      (list '+ a1 a2)]))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2))
      (* m1 m2)]
    [else (list '* m1 m2)]))

(define (sum? x)
  (and
    (pair? x)
    (eq? (car x) '+)))

(define (product? x)
  (and
    (pair? x)
    (eq? (car x) '*)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if
    (pair? s)
    (let
      [(augend (cddr s))]
      [foldr make-sum 0 augend])  ; foldr is the same as accumulate - to calculate the result with the operation and the first element, all the other elements on the right of it need to be calculated, like a carpet furled from the RIGHT to the left, ACCUMULATING more and more operation results.
    (caddr s)))

(define (multiplier p) (cadr p))
(define (multiplicant p)
  (if
    (pair? p)
    (let
      [(multiplicant (cddr p))]
      [foldr make-product 1 multiplicant])
    (caddr p)))

(define (base power)
  (cadr power))

(define (exponent power)
  (caddr power))

(define (exponentiation? x)
  (and
    (pair? x)
    (eq? (car x) '**)))

(define (make-exponentiation base exponent)
  (cond
    [(=number? exponent 1) base]
    [(=number? exponent 0) 1]
    [(=number? base 1) 1]
    [(=number? base 0) 0]
    [else (list '** base exponent)]))

(define (deriv expression var)
  (cond
    [(number? expression) 0]
    [(variable? expression)
      (if (same-variable? expression var) 1 0)]
    [(sum? expression)
      (make-sum
        (deriv (addend expression) var)
        (deriv (augend expression) var))]
    [(product? expression)
      (make-sum
        (make-product
          (multiplier expression)
          (deriv (multiplicant expression) var))
        (make-product
          (deriv (multiplier expression) var)
          (multiplicant expression)))]
    [(exponentiation? expression)
      (make-product                                      ; n*u^(n-1) * u'
        (make-product                                    ; n*u^(n-1)
          (exponent expression)                          ; n
          (make-exponentiation                           ; u^(n-1)
            (base expression)                            ; u
            (make-sum (exponent expression) -1)))        ; n + (-1) = n - 1
        (deriv (base expression) var))]                  ; u'
    [else
      (error "unknown expression type: DERIV" expression)]))



;; UNIT TESTING
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

(define exercise-test (test-suite
  "exercise 2.57 test suite"
  
  #:before (λ () (display "before test cases") (newline))
  #:after (λ () (display "after test cases") (newline))

  (test-case
    "test case for ..."
    (check-equal?
      (deriv '(x + 3) 'x)
      1
      "test case for ... failed"))

  (test-case
    "test case for deriving a sum with multiple operands"
    (check-equal?
      (deriv '(x + x + x + x + x) 'x)
      5
      "cannot sum with multiple operands correctly"))

  (test-case
    "test cse for product with multiple operands"
    (check-equal?
      (deriv '(2 * 2 * 6 * x) 'x)
      24
      "cannot multiply with multiple operands correctly"))

  (test-case
    "test case for combining sum and multiplication with multiple operands"
    (check-equal?
      (deriv '((2 * 5 * x) + (3 * x) + (4 * x) + (2 * x) + x) 'x)
      20
      "combination does not work correctly"))
  ))

(run-test-newlines exercise-test)
