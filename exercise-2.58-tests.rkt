#lang racket

(require rackunit)
(require "exercise-2.58.b-infix-representation-prevalenz.rkt")

;; ===== variable? =====
(test-case
  "variable? test case"
  (check-equal? (variable? 'x) true
    "variable? does not work correctly.")
  (check-equal?
    (variable? 'a) true
    "variable? does not work correctly.")
  (check-equal?
    (variable? 1) false
    "variable? does not work correctly."))

;; ===== same-variable? =====
(test-case
  "same-variable? test case"
  (check-equal? (same-variable? 1 1) false
    "same-variable? does not work correctly.")
  (check-equal? (same-variable? 'a 'b) false
    "same-variable? does not work correctly.")
  (check-equal? (same-variable? 'a 'a) true
    "same-variable? does not work correctly.")
  (check-equal? (same-variable? 'a 1) false
    "same-variable? does not work correctly."))

;; ===== =number? =====
(test-case
  "=number? test case"
  (check-equal? (=number? 'a 1) false
    "=number? does not work correctly.")
  (check-equal? (=number? 2 1) false
    "=number? does not work correctly.")
  (check-equal? (=number? 1 1) true
    "=number? does not work correctly.")
  (check-equal? (=number? 'a 'a) false
    "=number? does not work correctly."))

;; ===== make-sum =====
(test-case
  "make-sum test case"
  (check-equal? (make-sum 1 2) 3
    "make-sum does not work correctly.")
  (check-equal? (make-sum 'a 3) '(a + 3)
    "make-sum does not work correctly.")
  (check-equal? (make-sum 4 (make-sum 5 'b)) '(4 + (5 + b))
    "make-sum does not work correctly.")
  (check-equal? (make-sum 0 'c) 'c
    "make-sum does not work correctly.")
  (check-equal? (make-sum 'd 0) 'd
    "make-sum does not work correctly."))

;; ===== make-product =====
(test-case
  "make-product test case"
  (check-equal? (make-product 0 'a) 0
    "make-product does not work correctly.")
  (check-equal? (make-product 'b 0) 0
    "make-product does not work correctly.")
  (check-equal? (make-product 1 'c) 'c
    "make-product does not work correctly.")
  (check-equal? (make-product 'd 1) 'd
    "make-product does not work correctly.")
  (check-equal? (make-product (make-product 'e 1) 'c) '(e * c)
    "make-product does not work correctly."))

;; ===== sum? =====
(test-case
  "sum? test case"
  (check-equal? (sum? (make-sum 'a 'b)) true
    "sum? procedure does not work correctly.")
  (check-equal? (sum? (make-sum 1 'c)) true
    "sum? procedure does not work correctly.")
  (check-equal? (sum? (make-sum 2 3)) false
    "sum? procedure does not work correctly.")
  (check-equal? (sum? (make-sum (make-product 1 2) 3)) false
    "sum? procedure does not work correctly.")
  (check-equal? (sum? (make-product 2 3)) false
    "sum? procedure does not work correctly.")
  (check-equal? (sum? (make-product (make-sum 'd 4) 3)) false
    "sum? procedure does not work correctly."))

;; ===== product? =====
(test-case
  "product? test case"
  (check-equal?
    (product? (make-sum (make-product 'a 'b) 1)) false
    "product? procedure does not work correctly.")
  (check-equal?
    (product? (make-product (make-product 'a 'b) 1)) true
    "product? procedure does not work correctly.")
  (check-equal?
    (product? (make-product 2 'b)) true
    "product? procedure does not work correctly.")
  (check-equal?
    (product? (make-product 'a 'b)) true
    "product? procedure does not work correctly.")
  (check-equal?
    (product? (make-product 0 'b)) false
    "product? procedure does not work correctly."))

;; ===== exponentiation? =====
(test-case
  "exponentiation? test case"
  (check-equal? (exponentiation? '(a ** b)) true
    "exponentiation? predicate does not work correctly.")
  (check-equal? (exponentiation? '(1 ** b)) true
    "exponentiation? predicate does not work correctly.")
  (check-equal? (exponentiation? '(2 ** 3)) true
    "exponentiation? predicate does not work correctly."))

;; ===== addend =====
(test-case
  "addend test case"
  (check-equal? (addend '(1 + 2)) 1
    "addend procedure does not work correctly.")
  (check-equal? (addend '(a + 2)) 'a
    "addend procedure does not work correctly.")
  (check-equal? (addend '(b + c)) 'b
    "addend procedure does not work correctly."))

;; ===== augend =====
(test-case
  "augend test case"
  (check-equal? (augend '(a + 1 + 2)) '(1 + 2)
    "augend procedure does not work on sums of multiple operands correctly.")
  (check-equal? (augend '(a + 1 + b)) '(1 + b)
    "augend procedure does not work correctly.")
  (check-equal?
    (augend (make-sum 4 (make-product 'c 'd)))
    '(c * d)
    "augend procedure does not work correctly.")
  (check-equal?
    (augend '(a + (5 * 6) + 2))
    '((5 * 6) + 2)
    "augend procedure does not work correctly."))

;; ===== multiplier =====
(test-case
  "multiplier test case"
  (check-equal? (multiplier '(a * 1)) 'a
    "multiplier procedure does not work correctly.")
  (check-equal? (multiplier '(b * 2 * 3)) 'b
    "multiplier procedure does not work correctly.")
  (check-equal? (multiplier '(1 * c)) 1
    "multiplier procedure does not work correctly."))

;; ===== multiplicant =====
(test-case
  "multiplicant test case"
  (check-equal? (multiplicant '(a * 1 * 2)) '(1 * 2)
    "multiplicant procedure does not work on products of multiple operands correctly.")
  (check-equal? (multiplicant '(a * 1 * b)) '(1 * b)
    "multiplicant procedure does not work correctly.")
  (check-equal?
    (multiplicant (make-product 4 (make-sum 'c 'd)))
    '(c + d)
    "multiplicant procedure does not work correctly.")
  (check-equal?
    (multiplicant '(a * (5 + 6) * 2))
    '((5 + 6) * 2)
    "multiplicant procedure does not work correctly."))

;; ===== base =====
(test-case
  "base test case"
  (check-equal? (base '(2 ** 3)) 2
    "base selector does not work correctly.")
  (check-equal? (base '(a ** 2)) 'a
    "base selector does not work correctly."))

;; ===== exponent =====
(test-case
  "exponent test case"
  (check-equal? (exponent '(2 ** 3)) 3
    "exponent selector does not work correctly.")
  (check-equal? (exponent '(2 ** a)) 'a
    "exponent selector does not work correctly."))

;; ===== make-exponentiation =====
(test-case
  "make-exponentiation test case"
  (check-equal? (make-exponentiation 'a 1) 'a
    "make-exponentiation does not work correctly.")
  (check-equal? (make-exponentiation 'a 0) 1
    "make-exponentiation does not work correctly.")
  (check-equal? (make-exponentiation 'a 'b) '(a ** b)
    "make-exponentiation does not work correctly.")
  (check-equal? (make-exponentiation 1 'c) 1
    "make-exponentiation does not work correctly.")
  (check-equal? (make-exponentiation 0 0) 1
    "make-exponentiation does not work correctly."))

;; ===== deriv =====
(test-case
  "deriv test case"
  (check-equal? (deriv '(x + 3) 'x) 1
    "test case for simple addition derivate failed")
  (check-equal? (deriv '(x + x + x + x + x) 'x) 5
    "cannot sum with multiple operands correctly")
  (check-equal? (deriv '(2 * 2 * 6 * x) 'x) 24
    "cannot multiply with multiple operands correctly")
  (check-equal?
    (deriv '((5 * x) + (3 * x) + (4 * x) + (2 * x) + x) 'x)
    15
    "combination does not work correctly")
  (check-equal?
    (deriv '((2 * 5 * x) + (3 * x) + (4 * x) + (2 * x) + x) 'x)
    20
    "combination does not work correctly")
  (check-equal? (deriv '(2 + 2 * 5 * x) 'x) 10
    "combination does not work correctly")
  (check-equal? (deriv '((2 * 5 * x) + (3 + 10 * x)) 'x) 20
    "combination does not work correctly")
  (check-equal? (deriv (make-product (make-sum (make-product 10 3) 2) 'x) 'x) 32
    "combination does not work correctly")
  (check-equal? (deriv '((10 * 3 + 2) * x) 'x) '(10 * 3 + 2)
    "combination does not work correctly")
  (check-equal? (deriv '(x + 3 * (x + y + 2)) 'x) 4
    "combination does not work correctly")
  (check-equal? (deriv '((10 + 3 * x) + 3 * (x + y + 2)) 'x) 6
    "combination does not work correctly"))

;; ===== last-operation =====
(test-case
  "last-operation test case"
  (check-equal? (last-operation '(x * y + 1)) '+
    "last-operation selector does not work correctly.")
  (check-equal? (last-operation '((3 * 4) + 2)) '+
    "last-operation selector does not work correctly.")
  (check-equal? (last-operation '(1 + x * y)) '+
    "lastoperation selector does not work correctly.")
  (check-equal? (last-operation '(2 + (3 * z))) '+
    "last-operation selector does not work correctly.")
  (check-equal?
    (last-operation '(a + (b * 3 ** 4)))
    '+
    "last-operation selector does not work correctly.")
  (check-equal?
    (last-operation '(a + b * (3 ** 4)))
    '+
    "last-operation selector does not work correctly.")
  (check-equal?
    (last-operation '(a + b * 3 ** 4))
    '+
    "last-operation selector does not work correctly."))
