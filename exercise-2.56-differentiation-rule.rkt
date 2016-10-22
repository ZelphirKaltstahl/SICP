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

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (multiplier p) (cadr p))
(define (multiplicant p) (caddr p))

;; for exercise
(define (base power)
  (cadr power))

;; for exercise
(define (exponent power)
  (caddr power))

;; for exercise
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

;; EXAMPLES

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ (* x y) (* y (+ x 3))) 'x)
(deriv '(** (+ x 1) 2) 'x)
