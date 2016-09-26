#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; cons returns a function, which will apply another function to the initially
;; given x and y.
(define (cons x y)
  (lambda (m)
    (m x y)))

; car takes a function z, which it applies to
; another function, which always returns the first
; of two arguments it is given.

; Since z will be the function returned by cons,
; that function created by cons will apply the
; selection of first argument function to the pair
; of values already known from the original application
; of cons with the two parameters x and y
(define (car z)
  (z
    (lambda (p q) p)))

; so the cdr function must be something like
; the car function, but applying z to a function,
; which always returns the last value.
(define (cdr z)
  (z
    (lambda (p q) q)))

; testing it
(car (cons 1 2))
(car (cons 2 3))
(car (cons (cons 1 1) (cons 2 2)))
(car (car (cons (cons 1 1) (cons 2 2))))

(cdr (cons 1 2))
(cdr (cons 2 3))
(cdr (cons (cons 1 1) (cons 2 2)))
(cdr (cdr (cons (cons 1 1) (cons 2 2))))

; using the substitution model to show it works

; (car (cons 1 2))

; replace the inner-most cons call

; (car (lambda (m) (m 1 2)))

; replace the outer call of car
; note that the in the procedure car z
; is applied to the parameters, so the order gets reversed

; ((lambda (m) (m 1 2)) (lambda (p q) p))

; now the lambda expression is used instead of the parameter m

; ((lambda (p q) p) 1 2)

; the lambda expression takes the two
; values 1 and 2 or any symbols or objects
; and returns the first one

; (lambda (1 2) 1)

; the result is the first value

; 1
