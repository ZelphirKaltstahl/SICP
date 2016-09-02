#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; exercise 32
(define (accumulate combine neutral-element term next a b)
	(define (iter a result)
		(if
			(> a b)
			result
			(iter (next a) (combine (term a) result))))
	(iter a neutral-element))

(define (product-acc term next a b)
	(define (my-combine c d)
		(* c d))
	(accumulate my-combine 1 term next a b))

(define (sum-acc term next a b)
	(define (my-combine c d)
		(+ c d))
	(accumulate my-combine 0 term next a b))

(define (my-next n)
	(+ n 1))

(product-acc identity my-next 1 6)
(sum-acc identity my-next 1 10)