#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (square x) (* x x))

(define (derivative g)
	(let
		((h 0.000001))
		(lambda (x)
			(/
				(- (g (+ x h)) (g x))
				h))))

(define tolerance 0.0000000001)
(define (fixed-point f guess)
	(define (close-enough? value1 value2)
		(< (abs (- value1 value2)) tolerance))
	(define (try current-guess)
		(let
			((next (f current-guess)))
			(if
				(close-enough? current-guess next)
				next
				(try next))))
	(try guess))

;; to calculate g(x) = 0
;; newton transformation
(define (newton-transform g)
	(lambda (x)
		(- x (/ (g x) ((derivative g) x)))))


;; newton method
(define (newton-method g guess)
	; the g(x) = 0 (a zero of g) point can be calculated
	; by determining the fixed point of the
	; newton transformed function g, according to the book
	(fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
	(fixed-point (transform g) guess))

(define (sqrt n)
	(fixed-point-of-transform
		(lambda (x) (- (square x) n))
		newton-transform
		1.0))

(sqrt 2)