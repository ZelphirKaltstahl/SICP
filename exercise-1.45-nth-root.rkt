#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; GUESS FOR THE SOLUTION
; I suspect the answer to the question how many average damps are necessary to make a fixed point search for the n-th root converge to be logarithm with base 2 of n.
; * averaging sort of halves the distance between A and B
; * log2(1) is 0
; * log2(2) is exactly 1
; * log2(3) is 1.5849625007211563 so smaller 2
; * log2(4) is exactly 2
; These example values do at the least not contradict my guess.

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

(define (repeated func repeated-application-count)
	(display "repeating function: ") (display func) (newline)
	(define (iter result-function remaining-applications-count)
		(display "repeated: ") (display (- (+ repeated-application-count 1) remaining-applications-count)) (display " times") (newline)
		(if
			(= remaining-applications-count 1)
			result-function
			(iter
				(compose func result-function)
				(- remaining-applications-count 1))))
	(iter func repeated-application-count))

(define (average a b)
	(/ (+ a b) 2))

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (n-fold-average-damp f n)
	((repeated average-damp n) f))

;; square root average damped
(define (sqrt x)
	(fixed-point
		; (average-damp (sqrt-naive x))
		(n-fold-average-damp
			(lambda (y) (/ x y))
			1)
		1.0))

(define (logarithm-with-base base x)
	(/ (log x) (log base)))

(define (n-th-root n x)
	(define needed-average-damping (floor (logarithm-with-base 2 n)))
	; (define needed-average-damping 101)
	(fixed-point
		(n-fold-average-damp
			(lambda (y) (/ x (expt y (- n 1))))
			needed-average-damping)
		1.0))

(sqrt 9)
(display "now doing some testing for convergence ...") (newline)

;; TESTS

; (n-th-root 8 16777216)
; only converges if the needed-average-damping value is set to at least 3
; log2(8) = 3

; (n-th-root 7 823543)
; only converges if the needed-average-damping value is set to at least 2
; log2(7) >= 2
; log2(7) < 3

; (n-th-root 4 625)
; only converges if the needed-average-damping value is set to at least 2
; log2(2) = 2

; (n-th-root 100 (expt 10 200))
; only gives correct result if the needed-average-damping value is set to log2(100)

; The tests also seem to hint at log2(n).

(time (n-th-root 3 27))
(time (n-th-root 4 625))
(time (n-th-root 7 823543))
(time (n-th-root 8 16777216))
; (time (n-th-root 100 (expt 10 200)))

(display "all functions convergenced :)") (newline)