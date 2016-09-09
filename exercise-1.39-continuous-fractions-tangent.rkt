#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

; iterative process function
(define (cont-frac numerator-func denominator-func terms-limit combine-func)
	(define (calculate iteration current-result)
		(if
			(= iteration 0)
			current-result
			(calculate
				(- iteration 1)
				(/
					(begin
						; (display "numerator: ") (display (numerator-func iteration)) (newline)
						; (display "denominator: ") (display (denominator-func iteration)) (newline)
						; (display "---------------") (newline)
						(numerator-func iteration))
					(combine-func (denominator-func iteration) current-result)))))
	(calculate terms-limit 0))

;; exercise 1.39:
(define (series-element i)
	(+ (* 2 (- i 1)) 1))

(define (tan-cf x k)
	(display "calculating tan(")
	(display x)
	(display ") with k = ")
	(display k)
	(newline)
	(cont-frac
		(lambda (i) (if
			(= i 1)
			x
			(* x x)))
		(lambda (i) (series-element i))
		25
		-))

(define (deg-to-rad deg)
	(* (/ pi 180.0) deg))

(time (tan-cf (deg-to-rad 30.0) 25))
(time (tan-cf (deg-to-rad 60.0) 25))
(time (tan-cf (deg-to-rad 90.0) 25))