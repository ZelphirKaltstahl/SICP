#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; a) product iteratively
(define (product-iter term next a b)
	(define (iter a result)
		; (display "a: ") (display a) (newline)
		; (display "result: ") (display result) (newline)
		; (display "---------------") (newline)
		(if
			(> a b)
			result
			(iter (next a) (* (term a) result))))
	(iter a 1))

(define identity (lambda (n) n))
