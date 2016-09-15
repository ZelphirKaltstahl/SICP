#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (compose f g)
	(lambda (x)
		(f (g x))))

(define (square x)
	(* x x))

(define (inc x)
	(+ x 1))

((compose square inc) 6)