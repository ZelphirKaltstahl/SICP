#lang racket/base
 
(require
  rackunit
  "exercise-1.03.rkt")

(check-equal? (square 12) 144 "squaring")
(check-equal? (square (/ 11 10)) (/ 121 100) "squaring")
(check-within (square 1.1) 1.21 0.001)
(check-equal? (square 11) 121 "squaring")
(check-equal? (square 4) 16 "squaring")

(check-expect (+ 2 2) 4)

;(test-begin
;    (let ([lst (list 2 4 6 9)])
;        (check = (length lst) 4)
;        (for-each
;            (lambda (elt)
;                (check-pred even? elt))
;        lst)))