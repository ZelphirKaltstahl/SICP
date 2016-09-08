#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

; iterative process function
(define (cont-frac numerator-func denominator-func terms-limit)
	(define (calculate iteration current-result)
		(if
			(= iteration 0)
			current-result
			(calculate
				(- iteration 1)
				(/
					(numerator-func iteration)
					(+ (denominator-func iteration) current-result)))))
	(calculate terms-limit 0))

;; exercise 1.37.b:
; recursive process function
(define (cont-frac-rec numerator-func denominator-func terms-limit)
	(define (calculate iteration)
		(if
			(= iteration 0)
			0
			(/
				(numerator-func iteration)
				(+ 
					(denominator-func iteration)
					(calculate (- iteration 1))))))
	(calculate terms-limit))

;; exercise 1.37.a:
; we need 13 steps
(time (cont-frac
	(lambda (i) 1.0)
	(lambda (i) 1.0)
	13))

(time (cont-frac-rec
	(lambda (i) 1.0)
	(lambda (i) 1.0)
	13))

(time (cont-frac
	(lambda (i) 1.0)
	(lambda (i) 1.0)
	1000000))

(time (cont-frac-rec
	(lambda (i) 1.0)
	(lambda (i) 1.0)
	1000000))