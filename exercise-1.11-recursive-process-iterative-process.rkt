#lang racket

;; recursive process
(define (f n)
	(if
		(< n 3)
		n
		(+
			(f (- n 1))
			(* 2 (f (- n 2)))
			(* 3 (f (- n 3))))))

;; iterative process
(define (f-iter n)
	(if
		(< n 3)
		n
		(func-iter 2 1 0 n)))

(define (func-iter a b c count)
	(if
		(< count 3)
		a
		(func-iter
			; a = (+ a (* 2 b) (* 3 c))
			; variable a sums up everything
			(+ a (* 2 b) (* 3 c))
			a  ; b = a ??? because a was 
			b  ; c = b ???
			(- count 1))))  ; one less iteration remaining

;; SOLUTION
(define (F-iter n)
	(if
		(< n 3)
		n
		(F-iter-aux 2 1 0 n)))

(define (F-iter-aux a b c count)
	(if
		(= count 2)
		a
		(F-iter-aux
			(+ a (* 2 b) (* 3 c))
			a 
			b 
			(- count 1))))



(f 30)
(f-iter 30)
(F-iter 30)