#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; b) resursive process product
(define (product-rec term next a b)
	(if
		(> a b)
		1
		(* (term a) (product-rec term next (next a) b))))
;; b) end

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

;; task a: define factorial in terms of product
(define (factorial n)
	(define (next ni)
		(+ ni 1))
	(product-iter identity next 2 n))

;; define pi/4 approximation
; The book give the following approximation formula for pi/4:
;
;  pi     2*4*4*6*6*8* ...
; ---- = ------------------
;   4     3*3*5*5*7*7* ...
;
; this is equivalent to:
;
;  pi     2     4     4     6     6     8 
; ---- = --- * --- * --- * --- * --- * --- * ...
;   4     3     3     5     5     7     7 
;
; So we can formulate the following "next" rule:
;
; IF numerator < denominator THEN increase numerator by 2
; IF denominator < numerator THEN increate denominator by 2
;
; However, it would require us to access the numerator and denominator of a fraction. Alternatively we can define a different version of factorial, where the next function increases the argument by 2.

(define (even n)
	(= 0 (remainder n 2)))

(define (factorial-double-spacing begin end)
	(define (next ni)
		(+ ni 2))
	(product-iter identity next begin end))

(define (floor-to-even n)
	(- n (remainder n 2)))

(define (pi-quarter n)  ; n is the number of fractions being multiplied
	(define (numerator-max n) (if (even n) (+ n 2) (+ n 1)))  ; next even if not even
	(define (denominator-max n) (if (even n) (+ n 1) (+ n 2)))  ; next odd natural number
	(display "-----------------------------------")
	(newline)
	(display "numerator max for n = ")
	(display n)
	(display ": ")
	(display (numerator-max n))
	(newline)

	(display "odd:") (newline)
	(display "1. factor:")
	(display (factorial-double-spacing 2 (numerator-max n)))
	(newline)

	(display "2. factor:")
	(display (factorial-double-spacing 4 (numerator-max n)))
	(newline)

	(display "3. factor:")
	(display (factorial-double-spacing 3 (denominator-max n)))
	(newline)

	(display "4. factor:")
	(display (factorial-double-spacing 3 (- (denominator-max n) 2)))
	(newline)

	(display "even:") (newline)
	(display "1. factor:")
	(display (factorial-double-spacing 2 (- (numerator-max n) 2)))
	(newline)

	(display "2. factor:")
	(display (factorial-double-spacing 4 (numerator-max n)))
	(newline)

	(display "3. factor:")
	(display (factorial-double-spacing 3 (denominator-max n)))
	(newline)

	(display "4. factor:")
	(display (factorial-double-spacing 3 (denominator-max n)))
	(newline)

	(if
		(even n)
		(/
			(*
				(factorial-double-spacing 2 (- (numerator-max n) 2))
				(factorial-double-spacing 4 (numerator-max n)))
			(*
				(factorial-double-spacing 3 (denominator-max n))
				(factorial-double-spacing 3 (denominator-max n))))
		(/
			(*
				(factorial-double-spacing 2 (numerator-max n))
				(factorial-double-spacing 4 (numerator-max n)))
			(*
				(factorial-double-spacing 3 (denominator-max n))
				(factorial-double-spacing 3 (- (denominator-max n) 2))))))


(time (factorial-double-spacing 2 8))
(time (* (pi-quarter 5) 4))

(time (* (pi-quarter 6) 4))
; with product-iter: cpu time: 180 real time: 403 gc time: 3
; (only a one time measurement)

(time (* (pi-quarter 10000) 4))
; with product-rec: cpu time: 338 real time: 563 gc time: 131
; (only a one time measurement)