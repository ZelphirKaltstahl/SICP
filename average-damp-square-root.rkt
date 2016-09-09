#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (square x) (* x x))

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define tolerance 0.0000000001)
(define (fixed-point f guess)
	(define (close-enough? value1 value2)
		(< (abs (- value1 value2)) tolerance))
	(define (try current-guess)
		; (display current-guess) (newline)
		(let
			((next (f current-guess)))
			(if
				(close-enough? current-guess next)
				next
				(try next))))
	(try guess))

(define (sqrt x)
	(fixed-point
		(average-damp (lambda (y) (/ x y)))
		1.0))

(display "average damp square: ") (newline)
(time ((average-damp square) 10))
(newline)

(display "square root: ") (newline)
(time (sqrt 81))
(newline)