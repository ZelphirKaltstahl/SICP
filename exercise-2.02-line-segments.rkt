#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (average a b)
	(/ (+ a b) 2))

(define (make-point x y)
	(cons x y))

(define (x-of-point A)
	(car A))

(define (y-of-point A)
	(cdr A))

(define (make-segment A B)
	(cons A B))

(define (start-segment s)
	(car s))

(define (end-segment s)
	(cdr s))

(define (midpoint s)
	(let
		((x1 (x-of-point (start-segment s)))
		(y1 (y-of-point (start-segment s)))
		(x2 (x-of-point (end-segment s)))
		(y2 (y-of-point (end-segment s))))
		(make-point
			(average x1 x2)
			(average y1 y2))))

(define (print-point P)
	(display "(")
	(display (x-of-point P))
	(display "|")
	(display (y-of-point P))
	(display ")")
	(newline))

(display "point: ") (print-point (make-point 1.0 1.0))
(display "x of point: ") (display (x-of-point (make-point 1.0 1.0))) (newline)
(display "y of point: ") (display (y-of-point (make-point 1.0 1.0))) (newline)
(display "start of segment: ")
(print-point (start-segment (make-segment
	(make-point 1.0 1.0)
	(make-point 3.0 3.0))))
(display "end of segment: ")
(print-point (end-segment (make-segment
	(make-point 1.0 1.0)
	(make-point 3.0 3.0))))

(define (test seg)
	(print-point (start-segment seg))
	(print-point (end-segment seg))
	(print-point (midpoint seg)))

(test
	(make-segment
		(make-point 1.0 1.0)
		(make-point 3.0 3.0)))
