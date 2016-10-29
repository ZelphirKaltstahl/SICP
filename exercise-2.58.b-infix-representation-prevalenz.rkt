#lang racket

(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

(provide (all-defined-out))

;(provide
;  variable?
;  same-variable?
;  =number?
;  make-sum
;  make-product
;  make-exponentiation
;  operation
;  sum?
;  product?
;  exponentiation?
;  addend
;  augend
;  multiplier
;  multiplicant
;  base
;  exponent
;  deriv)

(define (take n a-list)
  (define (iter counter result sublist)
    (cond
      [(empty? sublist) result]
      [(< counter n)
       (iter
         (+ counter 1)
         (append result (list (car sublist)))
         (cdr sublist))]
      [else result]))
  (cond
    [(= n 0) '()]
    [else (iter 0 '() a-list)]))

(define (take-until a-list stop-elem)
  (define (iter result sublist)
    (cond
      [(empty? sublist) result]
      [(eq? (car sublist) stop-elem) result]
      [else (iter (append result (list (car sublist)))
                  (cdr sublist))]))
  (iter '() a-list))


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
    [(eq? a1 a2) (list 2 '* a1)]
    [else
      (list a1 '+ a2)]))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2))
     (* m1 m2)]
    [else (list m1 '* m2)]))

(define (make-exponentiation base exponent)
  (cond
    [(=number? exponent 1) base]
    [(=number? exponent 0) 1]
    [(=number? base 1) 1]
    [(=number? base 0) 0]
    [else (list base '** exponent)]))

;; EXPLANATION
;
;  Important is the lowest precedence operation.
;
;  If the lowest precedence operation is found, it can be split up into its respective pairs of addend and augend, multiplier and multiplicant or base and exponent.
;  The split will mean that the elements of the pairs are treated separately, which means that an operation of higher precedence will be treated separately, from operations of lower precedence, in a separate derivation call.
;  Only when the derivates of subterms of higher precedence "bubble back up" in the recursive calls as return values, the subterms of lower precedence can be derived, because they rely on these results.
;
;  Example:
;
;  (3 + 10 * x) ? SUM 3 AND 10 * x  | FIRST STEP
;  (10 * x) ? PRODUCT 10 AND X      | SECOND STEP
;
;  The example shows that the precedence is used in reversed.
;  This is reflected by the structure of the last-operation procedure.
(define (last-operation expression)
  (cond
    [(memq '+ expression) '+]
    [(memq '* expression) '*] 
    [(memq '** expression) '**]
    [else 'unknown]))

; a term is a sum, if the operation of lowest precedence is a sum, because that means, that the last operation applied will be the sum.
(define (sum? expression)
  (and
    (list? expression)
    (eq? (last-operation expression) '+)))

(define (product? expression)
  (and
    (list? expression)
    (eq? (last-operation expression) '*)))

(define (exponentiation? expression)
  (and
    (list? expression)
    (eq? (last-operation expression) '**)))



(define (addend s)
  (let
    [(raw-addend (take-until s '+))]
    [if (= (length raw-addend) 1)
      (car raw-addend)
      raw-addend]))

(define (augend s)
  (let
    [(augend-part (cdr (memq '+ s)))]
    [if (= (length augend-part) 1)
        (car augend-part)
        augend-part]))

(define (multiplier product)
  (let
    [(raw-multiplier (take-until product '*))]
    [if (= (length raw-multiplier) 1)
        (car raw-multiplier)
        raw-multiplier]))

(define (multiplicant p)
  (let
    [(multiplicant-part (cdr (memq '* p)))]
    [if (= (length multiplicant-part) 1)
      (car multiplicant-part)
      multiplicant-part]))

(define (base power)
  (car power))

(define (exponent power)
  (caddr power))


(define (deriv expression var)
  (display expression) (newline)
  (cond
    [(number? expression) 0]
    [(variable? expression)
     (if (same-variable? expression var) 1 0)]
    [(sum? expression)
     (display "sum recognized") (newline)
     (make-sum
       (deriv (addend expression) var)
       (deriv (augend expression) var))]
    [(product? expression)
     (display "product recognized") (newline)
     (make-sum
       (make-product
         (multiplier expression)
         (deriv (multiplicant expression) var))
       (make-product
         (deriv (multiplier expression) var)
         (multiplicant expression)))]
    [(exponentiation? expression)
     (display "exponentiation recognized") (newline)
     (make-product                                      ; n*u^(n-1) * u'
       (make-product                                    ; n*u^(n-1)
         (exponent expression)                          ; n
         (make-exponentiation                           ; u^(n-1)
           (base expression)                            ; u
           (make-sum (exponent expression) -1)))        ; n + (-1) = n - 1
       (deriv (base expression) var))]                  ; u'
    [else
      (error "unknown expression type: DERIV" expression)]))

