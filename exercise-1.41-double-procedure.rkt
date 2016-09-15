#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (double func)
	(lambda (x)
		(func (func x))))

(define (inc x)
	(+ x 1))

(display "(inc 5): ") (inc 5) (newline)
(display "((double inc) 5): ") ((double inc) 5) (newline)
(display "(((double double) inc) 5): ") (((double double) inc) 5) (newline)
(display "(((double (double double)) inc) 5): ") (((double (double double)) inc) 5) (newline)
