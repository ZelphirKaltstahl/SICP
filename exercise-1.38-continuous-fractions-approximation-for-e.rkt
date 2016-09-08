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



;; exercise 1.38:

; https://oeis.org/search?q=1%2C2%2C1%2C1%2C4%2C1%2C1%2C6%2C1%2C1%2C8&language=english&go=Search
; says that the continued fraction with D_i being:
; 1 0 1 1 2 1 1 4 1 1  6  1  1  8
; 0 1 2 3 4 5 6 7 8 9 10 11 12 13
; (floor ( 4 / 3)) * 2 = 2
; (floor ( 7 / 3)) * 2 = 4
; (floor (10 / 3)) * 2 = 6
; is e itself.
; 2.718 281 828 459 045 235 360 287 471 352 662 497 757 247 093 699 95...
; 1.718 281 828 459 045 5 for i = 100
(define (series-element i)
	(cond
		((= i 0) 1)
		((= i 1) 0)
		((= (remainder i 3) 1)
			(* (floor (/ i 3)) 2))
		(else 1)))

(time (+ 1 (cont-frac
	(lambda (i) 1.0)  ; N_i are all 1
	(lambda (i) (series-element i))  ; D_i are 1 2 1 1 4 1 1 6 1 1 8 ...
	25)))

;; Note
; Somehow the series given in the book does not work like this.
; Other solutions online often use 1 1 2 1 1 4 and so on as a series.
; This solution uses the series given by OEIS.