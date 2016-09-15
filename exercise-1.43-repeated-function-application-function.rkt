#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (square x)
	(* x x))

(define (compose f g)
	(lambda (x)
		(f (g x))))

(define (repeated func repeated-application-count)
	(define (iter result-function remaining-applications-count)
		(display "remaining-applications-count: ") (display remaining-applications-count) (newline)
		(if
			(= remaining-applications-count 1)
			result-function
			(iter
				(compose func result-function)
				(- remaining-applications-count 1))))
	(iter func repeated-application-count))

((repeated square 2) 5)