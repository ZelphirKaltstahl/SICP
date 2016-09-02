#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; prerequisites
(define (id n) n)
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))
(define (find-divisor n test-divisor)
	(cond
		((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n) (find-divisor n 2))

;; prime number test
(define (prime? n)
	(if
		(= n 1)
		false
		(= n (smallest-divisor n))))

;; exercise 1.33.a
(define (filter-accumulate filter combine neutral-element term next a b)
	(define (iter a result)
		(if
			(> a b)
			result
			(if
				(filter a)
				(begin
					(display "a:")
					(display a) (newline)
					(display "res:")
					(display result) (newline)
					(newline)
					(iter (next a) (combine (term a) result)))
				; do not combine if a does not satisfy the predicate
				(iter (next a) result))))
	(iter a neutral-element))

(define (always-true n) true)
(define (a-plus-b c d) (+ c d))
(define (simple-next n) (+ n 1))

;; definition of product using the filter-accumulate function
(define (product-acc a b)
	(filter-accumulate always-true a-plus-b 1 id simple-next a b))

;; definition of sum using the filter-accumulate function
(define (sum-acc a b)
	(filter-accumulate always-true a-plus-b 0 id simple-next a b))

;; definition of sum of primes using the filter-accumulate function
(define (sum-primes a b)
	(filter-accumulate prime? a-plus-b 0 id simple-next a b))

(time (product-acc 1 6))
(time (sum-acc 1 10))
(time (sum-primes 1 20))

;; exercise 1.33.b
(define (gcd a b)
	(if
		(= 0 (remainder a b))
		b
		(gcd b (remainder a b))))



(define (get-prime-rel-to n)
	; returns a function, which takes an i and checks for primality relative to n
	; returns a predicate
	(lambda (i) (= 1 (gcd i n))))

(define (product-primes-rel-to a b n)
	(define neutral-element 1)
	(filter-accumulate
		; filter             combine                neutral element term next        begin end
		(get-prime-rel-to n) (lambda (c d) (* c d)) neutral-element id   simple-next a     b))

(product-primes-rel-to 1 15 6)  ; should leave out all numbers which have 2, 3 or 6 in their factorization
