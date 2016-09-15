#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)


(define (derivative g)
	(let
		((h 0.000001))
		(lambda (x)
			(/
				(- (g (+ x h)) (g x))
				h))))

(define tolerance 0.000000000001)
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


(define (cubic a b c)
	(lambda (x)
		(+ (* x x x) (* a x x) (* b x) c)))

(time (newton-method
	(cubic 1 3 -2)
	-1.8))
