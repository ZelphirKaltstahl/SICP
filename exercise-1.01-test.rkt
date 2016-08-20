#lang racket/base
 
(require
  rackunit
  "exercise-1.03.rkt")

(check-equal? (square 12) 144 "squaring")
(check-equal? (square (/ 11 10)) (/ 121 100) "squaring")
(check-equal? (square 11) 121 "squaring")
(check-equal? (square 4) 16 "squaring")

(test-begin
    (let ([lst (list 2 4 6 9)])
        (check = (length lst) 4)
        (for-each
            (lambda (elt)
                (check-pred even? elt))
        lst)))