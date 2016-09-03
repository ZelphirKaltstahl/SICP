#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)


(define (close-enough? a b)
	(display "difference from 0 is now: ") (display (abs (- a b))) (newline)
	(< (abs (- a b)) 0.0001))

(define (average a b) (/ (+ a b) 2))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))

(define (search-zero f neg-point pos-point)
	(display "searching between ")
	(display neg-point)
	(display " (") (display (f neg-point)) (display ")")
	(display " and ")
	(display pos-point)
	(display " (") (display (f pos-point)) (display ")")
	(newline)
	(let
		((midpoint (average neg-point pos-point)))  ; calculate the midpoint dynamically
		(if
			(close-enough? neg-point pos-point)  ; if the difference is low enough
			midpoint  ; return the midpoint
			(let  ; else open a new let scope where we calculate further
				((test-value (f midpoint)))  ; calculate the function value of the midpoint
				(cond
					; if the function is continuous
					; there is a line between two points on the function value graph
					; we are searching for a point of the function where the function value is zero.
					; if we have a point, where the function value is positive
					; and we have a point, where the function is negative
					; there must be a crossing over the function value zero axis
					; if the function value at the midpoint is positive
					; then the crossing must be between this midpoint and the negative point
					; the midpoint becomes the next positive point
					((positive? test-value) (search-zero f neg-point midpoint))
					; if the function value at the midpoint is negative
					; then the crossing must be between this midpoint and the positive point
					; the midpoint becomes the next negative point
					((negative? test-value) (search-zero f midpoint pos-point))
					; if the function value is neither positive not negative
					; it must be zero and therefore is the null point we searched for
					(else midpoint))))))

; (search-zero
; 	(lambda (x) (- (* 2 x x) 2)) -20 20)

(define (half-interval-method f a b)
	(let
		((a-value (f a))
		(b-value (f b)))
		(cond
			((and
				(negative? a-value)
				(positive? b-value)) (search-zero f a b))
			((and
				(negative? b-value)
				(positive? a-value)) (search-zero f b a))
			(else (begin
				(display "function values of a and b are not of opposite sign")
				(newline)
				(display "f(a) = ")	(display (f a))
				(newline)
				(display "f(b) = ")	(display (f b))
				(newline))))))

; (half-interval-method
; 	(lambda (x) (- (* 2 x x) 2))
; 	9 -10)

(half-interval-method
	(lambda (x) (* (- x 1) (+ x 2) (- x 3)))
	-3.0 4.0)