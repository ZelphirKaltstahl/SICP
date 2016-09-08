#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define tolerance 0.0000000001)
(define (fixed-point f guess)
	(define (close-enough? value1 value2)
		(< (abs (- value1 value2)) tolerance))
	(define (try current-guess)
		(display current-guess) (newline)
		(let
			((next (f current-guess)))
			(if
				(close-enough? current-guess next)
				next
				(try next))))
	(try guess))

;; defines a value
(define golden-ratio
	(fixed-point
		(lambda (x) (+ 1 (/ 1 x)))
		1.0))

;; defines a function returning a value
; (define (golden-ratio)
; 	(fixed-point
; 		(lambda (x) (+ 1 (/ 1 x)))
; 		1.0))

;; prints the value
golden-ratio
;; prints the return value of the function
; (golden-ratio)