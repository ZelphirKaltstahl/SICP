#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (square x)
	(* x x))

(define (compose f g)
	(lambda (x)
		(f (g x))))

(define (repeated func repeated-application-count)
	(define (iter result-function remaining-applications-count)
		(display "remaining-applications-count: ") (display remaining-applications-count) (newline)
		(if
			(= remaining-applications-count 1)
			result-function
			(iter
				(compose func result-function)
				(- remaining-applications-count 1))))
	(iter func repeated-application-count)
	)

(define (average a b)
	(/ (+ a b) 2))

(define (average-three a b c)
	(/ (+ a b c) 3))

(define (smooth f)
	(define dx 0.1)
	(lambda (x)
		(average-three
			(f (+ x dx))
			(f x)
			(f (- x dx)))))

(define (n-fold-smooth f n)
	((repeated smooth n) f))


; (square 2)
; ((n-fold-smooth square 0) 2)
((smooth square) 2)
((n-fold-smooth square 1) 2)
((smooth (smooth square)) 2)
((n-fold-smooth square 2) 2)
((smooth (smooth (smooth square))) 2)
((n-fold-smooth square 3) 2)