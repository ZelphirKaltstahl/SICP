#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(car ''(abracadabra))

;; Explanation
;; We can replace one of the single quotes with (quote ...).
;; To show this we can try:

(car '(quote abracadabra))

;; The quote before a list allows to write the list without writing "list".
;; For example we could write into the REPL (list 1 2) and would get the result '(1 2).
;; We would get the same result if we wrote '(1 2).
;; This means '(quote abracadabra) will be a list of the identifier quote and abracadabra.
;; If we take the first element of that list, we get 'quote as a result.
;; It has the ' because it also marks that the procedure is not being called.
;; Same goes for a list being printed.
;; '(1 2) means that the procedure 1 is not being called.
;; Well it is not a procedure and thus we would not want to call it.
