#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define (interval-width interval)
  (/
    (abs
      (-
        (upper-bound interval)
        (lower-bound interval)))
    2))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval
    (- (upper-bound x) (lower-bound y))
    (- (lower-bound x) (upper-bound y))))
    
(define (mul-interval x y)
  (let
    [(p1 (* (lower-bound x) (lower-bound y)))
    (p2 (* (lower-bound x) (upper-bound y)))
    (p3 (* (upper-bound x) (lower-bound y)))
    (p4 (* (upper-bound x) (upper-bound y)))]
    [make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4)]))

;; EXERCISE 2.11

; It would be silly to reduce readability by distinguishing such a number of cases,
; for the sole purpose of saving a few multiplications!
; (skipped)
;
; The idea here is, that there are 3 cases for each of the intervals:
;
; * both bounds positive
; * both bounds negative
; * one negative one positive
;
; Since we need all cases, all combinations of those 3 cases per interval are needed.
; That makes 3*3=9 cases.
;
; To get a picture of the complications and unreadability check out this:
; http://eli.thegreenplace.net/2007/07/27/sicp-section-214
(define (mul-interval-cases x y)
  (cond
    ((predicate1)
      (consequent1))
    ((predicate2)
      (consequent2))
    ((predicate3)
      (consequent3))))

(define (div-interval x y)
  ;; EXERCISE 2.10
  ; exception handling
;  (with-handlers
;    ([exn:fail:contract:divide-by-zero?
;      (λ (exn) (raise exn))])
    
    (if (and
      (>= (upper-bound y) 0)
      (<= (lower-bound y) 0))
      
      (raise
        (make-exn:fail:contract:divide-by-zero
        "possible division by zero, interval spans zero"
        (current-continuation-marks)))
      
      (mul-interval
        x
        (make-interval
          (/ 1.0 (upper-bound y))
          (/ 1.0 (lower-bound y))))));)

(define (make-interval a b)
  (cons a b))

;; EXERCISE 2.08
; upper bound and lower bound procedures
(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (cdr interval) (car interval)))

;; UNIT TESTING
(define exercise-test
  (test-suite
    "exercise test suite"

    #:before (lambda () (begin (display "before")(newline)))
    #:after (lambda () (begin (display "after")(newline)))

    (test-case
      "checking the substraction of intervals"
      (check-equal?
        (sub-interval
          (make-interval 3.0 2.0)
          (make-interval 1.3 1.1))
        (make-interval 1.9 0.7)
        "substraction function not working properly"))

    (test-case
      "checking the lower bound"
      (check-equal?
        (lower-bound (make-interval 1.0 2.0))
        1.0
        "lower-bound not working correctly")
      (check-equal?
        (lower-bound (make-interval 2.0 1.0))
        1.0
        "lower-bound not working correctly"))

    (test-case
      "checking the upper bound"
      (check-equal?
        (upper-bound (make-interval 2.0 1.0))
        2.0
        "upper-bound not working correctly")
      (check-equal?
        (upper-bound (make-interval 2.0 1.0))
        2.0
        "upper-bound not working correctly"))

    (test-case
      "checking interval width"
      (check-equal?
        (interval-width (make-interval 2.0 1.0))
        0.5
        "interval-width not working correctly")
      (check-equal?
        (interval-width (make-interval -2.0 1.0))
        1.5
        "interval-width not working correctly"))

    (test-case
      "checking interval including zero"
      (check-exn
        exn:fail:contract:divide-by-zero?
        (λ () (div-interval
          (make-interval 1.0 2.0)
          (make-interval -3.0 2.0)))
        "division function does not properly raise division by zero exception")
      (check-exn
        exn:fail:contract:divide-by-zero?
        (λ () (div-interval
          (make-interval 1.0 2.0)
          (make-interval 3.0 -2.0)))
        "division function does not properly raise division by zero exception")
      (check-not-exn
        (λ () (div-interval
          (make-interval 1.0 2.0)
          (make-interval -3.0 -2.0)))
        "division function does not properly raise division by zero exception")
      (check-not-exn
        (λ () (div-interval
          (make-interval -1.0 2.0)
          (make-interval 3.0 2.0)))
        "division function does not properly raise division by zero exception")
      (check-not-exn
        (λ () (div-interval
          (make-interval 1.0 -2.0)
          (make-interval 3.0 2.0)))
        "division function does not properly raise division by zero exception")
      (check-not-exn
        (λ () (div-interval
          (make-interval -1.0 -2.0)
          (make-interval 3.0 2.0)))
        "division function does not properly raise division by zero exception"))))

(run-test exercise-test)

;; EXERCISE 2.09
; testing some interval widths
(interval-width (make-interval -3.0 1.2))
(interval-width (make-interval 5.0 -2.4))
(interval-width (make-interval -5.0 -2.4))
(interval-width (make-interval 5.0 2.4))

;; Addition
(interval-width
  (add-interval
    (make-interval -3.0 1.2)
    (make-interval 5.0 -2.4)))

(interval-width
  (add-interval
    (make-interval 3.0 1.2)
    (make-interval 5.0 2.4)))

; in addition the widths seem to add

;; Substraction
(interval-width
  (sub-interval
    (make-interval -3.0 1.2)
    (make-interval 5.0 -2.4)))

(interval-width
  (sub-interval
    (make-interval 3.0 1.2)
    (make-interval 5.0 2.4)))

; in substraction the widths seem to add

;; Multiplication
(display "Multiplication:") (newline)
(display "Interval 1: ") (interval-width (make-interval 1.5 2.0))
(display "Interval 2: ") (interval-width (make-interval 3.0 1.5))
(display "Product: ")
(interval-width
  (mul-interval
    (make-interval 1.5 2.0)
    (make-interval 3.0 1.5)))
(newline)

(display "Interval 1: ") (interval-width (make-interval 1.5 2.0))
(display "Interval 2: ") (interval-width (make-interval 3.0 2.0))
(display "Product: ")
(interval-width
  (mul-interval
    (make-interval 1.5 2.0)
    (make-interval 3.0 2.0)))
(newline)
    
(display "The resulting interval width is not simply a sum, difference or product of the factors' interval widths.") (newline) (newline)

;; Division
(display "Division:") (newline)
(display "Interval 1: ") (interval-width (make-interval 1.5 2.0))
(display "Interval 2: ") (interval-width (make-interval 3.0 1.5))
(display "Result: ")
(interval-width
  (div-interval
    (make-interval 1.5 2.0)
    (make-interval 3.0 1.5)))
(newline)

(display "Interval 1: ") (interval-width (make-interval 1.5 2.0))
(display "Interval 2: ") (interval-width (make-interval 3.0 2.0))
(display "Result: ")
(interval-width
  (div-interval
    (make-interval 1.5 2.0)
    (make-interval 3.0 2.0)))
(newline)

(display "The resulting interval width is not always simply a sum, difference or product of the factors' interval widths.") (newline) (newline)
