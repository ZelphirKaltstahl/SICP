#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (cdr interval) (car interval)))

(define (interval-width interval)
  (/ (abs (-
    (upper-bound interval)
    (lower-bound interval)))
    2))

(define (make-interval a b)
  (cons a b))

;; NEW PROCEDURES
(define (average a b)
  (/ (+ a b) 2))

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (center interval)
  (average (lower-bound interval) (upper-bound interval)))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

;; EXERCISE 2.12
(define (make-center-percent c p)
  (let
    [(upper (* c (+ 1.0 p)))
    (lower (* c (- 1.0 p)))]
    [make-interval upper lower]))

(define (percent interval)
  (let
    [(c (center interval))
    (diff
      (abs (-
        (center interval)
        (upper-bound interval))))]
    [round-off
      (* (/ 100.0 c) diff)
      4]))

(define (approx-equal a b tolerance)
  (<= (abs (- a b)) tolerance))

;; UNIT TESTING
(define exercise-test
  (test-suite
    "exercise test suite"

    #:before (lambda () (begin (display "before")(newline)))
    #:after (lambda () (begin (display "after")(newline)))

    (test-case
      "checking the percent selector"
      (check-true (approx-equal
        (percent (make-interval
          (+ 1.0 (* 0.1 1.0))
          (- 1.0 (* 0.1 1.0))))
        10.0
        0.0001)
        "percent function not working properly"))

    (test-case
      "checking approx-equal"
      (check-true
        (approx-equal 1.0002 1.0003 0.0001)
        "approx-equal function not working properly")
      (check-true
        (approx-equal -1.0002 -1.0003 0.0001)
        "approx-equal function not working properly"))))

(run-test exercise-test)

(percent (make-interval 1.1 0.9))
