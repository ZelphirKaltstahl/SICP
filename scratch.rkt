#lang racket
(define (fib n)
	(fib-iter 1 0 n))

(define (fib-iter a b count)
	(if
		(= count 0)
		b
		(fib-iter (+ a b) a (- count 1))))

(define (fib-rec n)
	(cond
		((= n 0) 1)
		((= n 1) 1)
		(else (+ (fib-rec (- n 1)) (fib-rec (- n 2))))))

(print "iterative")
(time (fib 100))

(print "recursive")
(time (fib-rec 100))

