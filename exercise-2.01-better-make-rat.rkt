#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (gcd a b)
	(if
		(= b 0)
		a
		(gcd b (remainder a b))))

(define (neg n)
	(* -1 n))

(define (make-rat n d)
	(cond
		((and (< n 0) (>= d 0))
			(begin
				;(display "n<0, d>=0")
				(let
					((g (gcd (neg n) d)))
					(cons (/ n g) (/ d g)))))

		((and (>= n 0) (< d 0))
			(begin
				;(display "n>=0, d<0")
				(let
					((g (gcd n (neg d))))
					(cons (/ (neg n) g) (/ (neg d) g)))))

		((and (< n 0) (< d 0))
			(let
				((g (gcd (neg n) (neg d))))
				(cons (/ (neg n) g) (/ (neg d) g))))

		(else
			(let
				((g (gcd n d)))
				(cons (/ n g) (/ d g))))))

(define numer car)
(define denom cdr)

(define (add-rat x y)
	(make-rat
		(+
			(* (numer x) (denom y))
			(* (numer y) (denom x)))
		(*
			(denom x)
			(denom y))))

(define (sub-rat x y)
	(make-rat
		(-
			(* (numer x) (denom y))
			(* (numer y) (denom x)))
		(*
			(denom x)
			(denom y))))

(define (mul-rat x y)
	(make-rat
		(* (numer x) (numer y))
		(* (denom x) (denom y))))

(define (div-rat x y)
	(make-rat
		(* (numer x) (denom y))
		(* (denom x) (numer y))))

(define (equal-rat? x y)
	(=
		(* (numer x) (denom y))
		(* (numer y) (denom x))))


(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

(display "calculating: ")
(print-rat (mul-rat (make-rat 10 3) (make-rat 4 5)))
(print-rat (mul-rat (make-rat -10 3) (make-rat 4 5)))
(print-rat (mul-rat (make-rat 10 -3) (make-rat 4 5)))
(print-rat (mul-rat (make-rat -10 -3) (make-rat 4 5)))