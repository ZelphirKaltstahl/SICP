#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (close-enough? a b)
	(define tolerance 1.e-8)
	(< (abs (- a b)) tolerance))

(define (average a b)
	(/ (+ a b) 2))

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (iterative-improve guess-good-enough? improve)
	(lambda (x)
		(let
			((improved-x (improve x)))
			(if
				(guess-good-enough? x improved-x)
				; if the guess is good enough, we return the guess
				improved-x
				; if the improved guess is not good enough yet, we do another iteration
				((iterative-improve guess-good-enough? improve) improved-x)))))

; exercise 1.46.a
(define (sqrt-iter-improve n)
	((iterative-improve
		close-enough?
		(lambda (guess)
			(average guess (/ n guess))))
	1.0))

(sqrt-iter-improve 9)

; exercise 1.46.b
(define (fixed-point f guess)
	((iterative-improve
		close-enough?
		; fixed point search of a function is an example of iterative improvement
		; we apply f over and over again on the result of the previous function application
		; in order to get to make the (function of) results converge
		; since iterative improvement does exactly that, we only need to give f as a parameter
		f)
	guess))

(define (sqrt-fp-iter-improve x)
	(define (improve x)
		(average-damp (lambda (y) (/ x y))))
	((iterative-improve
		close-enough?
		; here we must apply improve instead of only giving the function's name as a parameter,
		; because the lambda inside the improve function must already be average damped,
		; when it is used in the iterative improvement function
		(improve x))
	1.0))

(sqrt-fp-iter-improve 9)