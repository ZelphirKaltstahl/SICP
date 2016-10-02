#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...


;(define (f x y . z)
;  (cons x (cons y z)))

(define (same-parity first . others)
  (define (parity number)
    (remainder number 2))
  
  (define (equal-parity? a b)
    (= (parity a) (parity b)))
  
  (define (iter sublist reslist)
    (if
      (empty? sublist)
      (reverse reslist)
      (if
        (equal-parity? (car sublist) first)
        (iter (cdr sublist) (cons (car sublist) reslist))
        (iter (cdr sublist) reslist))))
  (iter others (list first)))

(time (same-parity 1 2 3 4 5 6 7))
(time (same-parity 2 3 4 5 6 7 8))
