#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; given church numeral 0
; zero means apply a function zero times
; λf.λx.x = λfx.x
(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; Church Numeral 1 would be applying a function once:
; λf.λx.fx = λfx.fx
(define one
  (lambda (f)
    (lambda (x)
      (f x))))

; Church Numeral 2 would be applying a function twice:
; λf.λx.f(fx)
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

; plus function means to apply a function m + n times
; λm.λn.λf.λx.((m f) ((n f) x)) = λmnfx.m f (n f x)
(define (plus m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

; predecessor is difficult
; PRED :=
;   λn.λf.λx.
;     n (λg.λh.h (g f)) (λu.x) (λu.u)

;   (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) 3
; = λf.λx.3 (λg.λh.h (g f)) (λu.x) (λu.u)
; = λf.λx. (λh.h (3 f)) (λu.x) (λu.u)
; = λf.λx. (λu.x) (3 f) (λu.x) (λu.u)

(define (square x)
  (* x x))

(time (((plus two two) square) 3))

; racket -l errortrace -t exercise-...
