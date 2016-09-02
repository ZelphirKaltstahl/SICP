#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (sum-iter term next a b)
	(define (iter a result)
		(if
			(> a b)
			result
			(iter (next a) (+ (term a) result))))
	(iter a 0))