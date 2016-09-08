#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define tolerance 0.00001)
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

; (fixed-point cos 1.0)
; (fixed-point sin 1.0)
; (fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)

(define (average a b)
	(/ (+ a b) 2))

; search for square root
(define (sqrt-not-converging x)
	(fixed-point (lambda (y) (/ x y)) 1.0))

(define (sqrt x)
	(fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Explanation:
; The book states, that the answer is always between the guess and the result of the function application. It is because of this, that we can get closer to the answer by taking the average of the guess and the result of the function application.
; However, why is that the case? Why is the answer always between the guess and the result of function application?
; A square root is sort of "half way to a number when multiplying".
; If we divide a number n for which we search the square root s, by a number g_1 > s, then we will get a result r_1 < s. The r_1 cannot be > s, because then both numbers would be greater than the square root of n and it would mean, that one can multiply two numbers, which are both greater than another number n, but still get the same result as if one multiplied that number n by itself.
; If we divide n by a number g_2 < s, we will thus get a number r_2 > s.
; If we divide n by a number, which is equal s, then the function fixed-point would recognize this number as "close-enough?" to the root and we would be done.
; So the answer (the square root) must be between the guess and the result of function application, which means we can get closer to the answer, by averaging the two.
; The average of the two numbers is closer to the guess, than the result of function application is, simply because it is in between those two.
; This means we make smaller steps towards the square root.
; This is what the book suggests as a way to solve the oscillation problem: "One way to control such oscillation is to prevent the guesses from changing so much."
; Instead of one way to converge to the square root, we use another, which we can get from the original formula by transforming it as described in the book. (+y; /2 to both sides of the equation)

(sqrt 1)
(sqrt 4)
(sqrt 9)
(sqrt 16)
(sqrt 25)
(sqrt 36)
