#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define x (cons 1 2))

(car x)
(cdr x)

(define a (cons 1 2))
(define b (cons 3 4))

(define z (cons a b))

(car (car z))
(cdr (car z))
(car (cdr z))
(cdr (cdr z))

(define (gcd a b)
	(if
		(= b 0)
		a
		(gcd b (remainder a b))))

(define (make-rat n d)
	(let
		((g (gcd n d)))
		(cons (/ n g) (/ d g))))

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
(print-rat (make-rat 10 3))
(display " * ")
(print-rat (make-rat 4 5))
(newline)
(print-rat (mul-rat (make-rat 10 3) (make-rat 4 5)))
