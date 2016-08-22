#lang racket

(define (sum-iter term next a b)
	(define (iter a result)
		(if
			(> a b)
			result
			(iter (next a) (+ (term a) result))))
	(iter a 0))

;; general summation of terms
(define (sum term next a b)
	(if
		(> a b)
		0
		(+ (term a) (sum term next (next a) b))))

;; sum of cubes
(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(display "sum of cubes: ")
(sum cube inc 0 10)

;; sum of integers
(define identity (lambda (n) n))

(display "sum of integers: ")
(sum identity inc 0 10)

;; sum of squares
(display "sum of squares: ")
(sum
	(lambda (n) (* n n))
	inc
	0
	10)

;; pi sum
(define (pi-sum a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term pi-next a b))

(display "PI approximation with 1*10^6: ")
(* 8 (pi-sum 1 1000000))

;; integral approximation for small values of dx
;; the smaller the value, the more precise the result
(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(*
		(sum
			f  ; term
			add-dx  ; next function
			(+ a (/ dx 2.0))  ; new a becomes the old a plus dx/2
			b)  ; b stays b as an upper boundary
		dx))

(display "integral of square from 0 to 1: ")
(time (integral
	(lambda (x) (* x x))
	0
	1
	0.000001))

;; Simpson'S rule
(define (next-even n) (+ n (remainder n 2)))

(define (simpsons-rule f a b n)
	; setting h and n for the rest of the calculation
	(define even-n (next-even n))
	(define h (/ (- b a) even-n))

	(define (next kh)
		(+ kh (* 2 h)))

	(* (/ h 3) (+
		;; add the term for k = 0
		; k is zero for y_0, the mathematical expression becomes f(a + 0h), which is equal to f(a)
		(f a)
		;; sum all the expressions for odd k and multiply by 4
		; start with a = a+1, the a = given a was already added in the previous summand
		; we need to substract 1h, because the sum function goes from a to b
		; and the next function always adds in units of h to a
		; and for the last part of the sum where k = n, we have a separate term we add
		(* 4 (sum-iter f next (+ a h) (- b h)))
		;; sum all the expressions for even k
		(* 2 (sum-iter f next (+ a (* 2 h)) (- b h)))
		;; add the term for k = n
		; with k = n, the mathematical expression f(a + kh), which is
		; equal to f(a + k*((b-a) / n)) becomes
		; f(a + n*((b-a) / n)), which is equal to
		; f(a + (b-a)), which is equal to f(b)
		(f b))))
		

(display "n = 10:") (newline)
(time (simpsons-rule
	(lambda (x) (* x x))
	0
	1
	10))

(time (integral
	(lambda (x) (* x x))
	0
	1
	0.1))

(display "n = 100:") (newline)
(time (simpsons-rule
	(lambda (x) (* x x x))
	0
	1
	100))

(time (integral
	(lambda (x) (* x x x))
	0
	1
	0.01))

(display "n = 1000:") (newline)
(time (simpsons-rule
	(lambda (x) (* x x x))
	0
	1
	1000))

(time (integral
	(lambda (x) (* x x x))
	0
	1
	0.001))

;; Note
; I did not discover the mathematical trick used in this exercise, which is splitting the sum into 4 parts.
; I took that idea from http://community.schemewiki.org/?sicp-ex-1.29.
; After getting the idea from there and performing the mathematical operations,
; in order to simplify the sum myself, to arrive at the 4 parts form,
; I was able to implement it and even improve upon the solution given in the schemewiki,
; by further simplifying the y_n part of the sum to f(b) and not only f(a + hn) as given in the wiki.

;; Answer

; for the square function:
; The simpsons-rule function calculates the result correctly,
; at least for the square function and intervals [0;1] and [0;2],
; while the integral function shows rounding errors.

; for the cube function: