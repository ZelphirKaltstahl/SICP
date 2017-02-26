#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; EXERCISE 2.73
;; =============
;; This is the old code for symbolic derivation.
(define (deriv expression var)
  ;; derives an expression with regard to the (mathematical) variable var
  (cond
    [(number? expression) 0]
    [(variable? expression)
      (if
        (same-variable? expression var)
        1
        0)]
    [(sum? expression)
      (make-sum
        (deriv (addend expression) var)
        (deriv (augend expression) var))]
    [(product? expression)
      (make-sum
        (make-product
          (multiplier expression)
          (deriv (multiplicand expression) var))
        (make-product
          (deriv (multiplier expression) var)
          (multiplicand expression)))]
    ;; more rules can be added here
    [else
      (error "unknown expression type: DERIV" expression)]))

;; Using data-directed programming as an approach, we can transform it into:
(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else
      ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))

;; a. Explain what was done above. Why can’t we assimilate the predicates number? and variable? into the data-directed dispatch?

;; a.1.
;; The conditional statement was exchanged with a lookup in the table of operations to get a suitable operation for dealing with the expression.
;; This shortens the code significantly, because the logic how these operations are implemented is defined in the operations registered in the table of operations and not our current concern.
;; It also has the advantage of being able to separate concerns. One person might be responsible for writing the procedure for deriving a sum, another person might be responsible for writing the procedure for deriving a product and another one for a division.

;; a.2.
;; A derivation procedure could rely on their own representation of numbers. (?)
;; Maybe someone implements the derivation of complex numbers for example.
;; If we implemented this, we would have to change it, whenever someone implements an operation for a new representation of numbers.

;; b. Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

(define (install-sum-package)

  ;; internal procedures
  (define (addend expression)
    (second expression))
  (define (augend expression)
    (third expression))
  (define (multiplier p)
    (cadr p))
  (define (multiplicant p)
    (caddr p))
  
  (define (derive-sum expression deriv-var)
    (make-sum
      (deriv (addend expression) var)
      (deriv (augend expression) var)))
  (define (derive-product expression deriv-var)
    ;; math: derive u*v = u'v + uv'
    (make-sum
      (make-product
        (deriv (multiplier expression) deriv-var)
        (multiplicant expression))
      (make-product
        (multiplier expression)
        (deriv (multiplicant expression) deriv-var))))

  ;; interface to the rest of the system
  (define (make-sum a b)
    (cond
      [(and (number? a) (number? b)) (+ a b)]
      [(= a 0) b]
      [(= b 0) a]
      [(and (= a b) (number? a) (number? b)) (make-product 2 a)]
      [else (attach-tag '+ (list a b))]))
  
  (define (make-product a b)
    (cond
      [(and (number? a) (number? b)) (* a b)]
      [(or (= a 0) (= b 0)) 0]
      [(= a 1) b]
      [(= b 1) a]
      [else (attach-tag '* (list a b))]))

  ;; add the operation derive-sum to the operations table for the operation "deriv" of data tagged with "+"
  ;; same for derive-product respectively to derivatives of products
  (put 'deriv (list '+) derive-sum)
  (put 'deriv (list '*) derive-product)
  'done)

;; c. Choose any additional differentiation rule that you like, such as the one for exponents (Exercise 2.56), and install it in this data-directed system.
(define (install-sum-package)
  (define (base power)
    (cadr power))
  (define (exponent power)
    (caddr power))
  
  (define (derive-power expression deriv-var)
    (let
      ((the-base (base expression))
       (the-exponent (exponent expression)))
      (cond
        [(eq? the-exponent deriv-var)
          ;; b^x --> b^x * ln(b), x is deriv var
          (make-product expression (make-log the-base))]
        [(eq? the-base deriv-var)
          (make-product
            the-exponent
            (make-exponentiation the-base (- the-exponent 1)))]
        [(and (number? the-base) (number? the-exponent))
          (expt the-base the-exponent)]
        [else (error "do not know how to derive expression" expression)])))

  (define (make-log x)
    (attach-tag 'ln (list x)))
  
  (define (make-exponentiation base exponent)
    (cond
      [(= exponent 0) 1]
      [(= base 1) 1]
      [(= exponent 1) base]
      [else (attach-tag '** (list b x))]))
  
  (put 'deriv (list '**) derive-power)
  'done)

;; d. In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like:

;; ((get (operator exp) 'deriv) (operands exp) var)
;; What corresponding changes to the derivative system are required?

;; The order of arguments to the procedure put. We would simply fill the table with inverted dimensions. First the type of expression, then the operation we want to do on them.
