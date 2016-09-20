#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

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
