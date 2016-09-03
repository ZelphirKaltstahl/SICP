#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (square x) (* x x))
(define (f g) (g 2))

(f square)
; 4
(f (lambda (z) (* z (+ z 1))))
; 6

(f f)
;; explanation:
; f (f1) gets the function f (f2) as a parameter, but f2 does not have any parameters. My guess is, that because there are no parameters for f2, instead of applying a given g to 2, it applies a no action on the 2, which results in 2. This 2 is then used as a parameter for f1, supposedly as a function, but it cannot be applied to 2, so we get the error:

; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: 2
;   arguments...:
;    2
;   context...:
;    /.../SICP/exercise-1.34-f-f.rkt: [running body]