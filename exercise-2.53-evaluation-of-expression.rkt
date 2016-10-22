#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...


; given code
(define (memq item x)
  (cond
    [(empty? x) #f]  ; if the list is empty, return false
    [(eq? item (car x)) x]  ; if the first item of the remaining list consists of the same characters as x return the remaining list
    [else (memq item (cdr x))]))  ; otherwise discard the first element and recurse

; exercise expressions
(list 'a 'b 'c)
(list (list 'george))

(car '((x1 x2) (y1 y2)))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
