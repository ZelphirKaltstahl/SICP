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

(define (print-segment seg)
	(print-point (start-segment seg))
	(print-point (end-segment seg)))

(define (square x)
	(* x x))

(define (absdiff a b)
	(abs (- a b)))

(define (seg-length seg)
	(let
		((start (start-segment seg))
		(end (end-segment seg)))
		(let
			((a (absdiff (x-of-point start) (x-of-point end)))
			(b (absdiff (y-of-point start) (y-of-point end))))
			(sqrt (+ (square a) (square b))))))

(define (perimeter rect)
	(+
		(seg-length (upper-side rect))
		(seg-length (right-side rect))
		(seg-length (bottom-side rect))
		(seg-length (left-side rect))))

(define (area rect)
	(*
		(seg-length (upper-side rect))
		(seg-length (left-side rect))))

; Here start the procedures depending on how the rectangle's representation is implemented.
; They represent the abstraction barrier as far as I understand.

; Representation 1
;(define (upper-side rect)
;	(make-segment
;		(make-point
;			(x-of-point (car rect))
;			(y-of-point (cdr rect)))
;		(cdr rect)))
;
;(define (right-side rect)
;	(make-segment
;		(cdr rect)
;		(make-point
;			(x-of-point (cdr rect))
;			(y-of-point (car rect)))))
;
;(define (bottom-side rect)
;	(make-segment
;		(car rect)
;		(make-point
;			(x-of-point (cdr rect))
;			(y-of-point (car rect)))))
;
;(define (left-side rect)
;	(make-segment
;		(make-point
;			(x-of-point (car rect))
;			(y-of-point (cdr rect)))
;		(car rect)))
;
;; This representation uses 2 corners on a diagonal.
;(define (make-rect lower-left-corner upper-right-corner)
;	(cons lower-left-corner upper-right-corner))

; Representation 2
; convenience procedures
(define (upper-left-corner rect)
	(car rect))

(define (upper-right-corner rect)
	(car (cdr rect)))

(define (bottom-right-corner rect)
	(car (cdr (cdr rect))))

(define (bottom-left-corner rect)
	(cdr (cdr (cdr rect))))

; changed procedures
(define (upper-side rect)
	(make-segment
		(upper-left-corner rect)
		(upper-right-corner rect)))

(define (right-side rect)
	(make-segment
		(upper-right-corner rect)
		(bottom-right-corner rect)))

(define (bottom-side rect)
	(make-segment
		(bottom-right-corner rect)
		(bottom-left-corner rect)))

(define (left-side rect)
	(make-segment
		(bottom-left-corner rect)
		(upper-left-corner rect)))

; This representation uses 4 corners in a linked list.
(define (make-rect upper-left upper-right lower-right lower-left)
	(cons upper-left
		(cons upper-right
			(cons lower-right lower-left))))

; test procedures
;(define (test rect)
;	(display "upper side:") (newline)
;	(print-segment (upper-side rect))
;	(display "right side:") (newline)
;	(print-segment (right-side rect))
;	(display "bottom side:") (newline)
;	(print-segment (bottom-side rect))
;	(display "left side:") (newline)
;	(print-segment (left-side rect))
;
;	(display "length upper side: ")
;	(display (seg-length (upper-side rect))) (newline)
;	(display "length right side: ")
;	(display (seg-length (right-side rect))) (newline)
;	(display "length bottom side: ")
;	(display (seg-length (bottom-side rect))) (newline)
;	(display "length left side: ")
;	(display (seg-length (left-side rect))) (newline)
;
;	(display "area: ")
;	(display (area rect)) (display " AU") (newline)
;
;	(display "perimeter: ")
;	(display (perimeter rect)) (display " LU"))
;
;(test
;	(make-rect
;		(make-point 1.0 1.0)
;		(make-point 3.0 6.0)))

(define (test rect)
	(display "upper side:") (newline)
	(print-segment (upper-side rect))
	(display "right side:") (newline)
	(print-segment (right-side rect))
	(display "bottom side:") (newline)
	(print-segment (bottom-side rect))
	(display "left side:") (newline)
	(print-segment (left-side rect))

	(display "length upper side: ")
	(display (seg-length (upper-side rect))) (newline)
	(display "length right side: ")
	(display (seg-length (right-side rect))) (newline)
	(display "length bottom side: ")
	(display (seg-length (bottom-side rect))) (newline)
	(display "length left side: ")
	(display (seg-length (left-side rect))) (newline)

	(display "area: ")
	(display (area rect)) (display " AU") (newline)

	(display "perimeter: ")
	(display (perimeter rect)) (display " LU"))

(test
	(make-rect
		(make-point 1.0 3.0)
		(make-point 3.0 3.0)
		(make-point 3.0 1.0)
		(make-point 1.0 1.0)))

; The procedures `area` and `perimeter` have not changed, although the representation of the square changed.
