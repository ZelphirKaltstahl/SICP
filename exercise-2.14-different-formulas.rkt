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

(define (div-interval x y)
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
        (/ 1.0 (lower-bound y))))))



(define (average a b)
  (/ (+ a b) 2))

(define (approx-equal a b tolerance)
  (<= (abs (- a b)) tolerance))

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))



(define (center interval)
  (average (lower-bound interval) (upper-bound interval)))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

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



(define (make-interval a b)
  (cons a b))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (let
    [(upper (* c (+ 1.0 (/ p 100))))
    (lower (* c (- 1.0 (/ p 100))))]
    [make-interval upper lower]))


;; EXERCISE 2.14
; numerical stability

;; (R1*R2)/(R1+R2)
(define (par1 r1 r2)
  (div-interval
    (mul-interval r1 r2)
    (add-interval r1 r2)))

;; 1 / ((1/R1) + (1/R2))
(define (par2 r1 r2)
  (let
    [(one (make-interval 1 1))]
    [div-interval
      one
      (add-interval
        (div-interval one r1)
        (div-interval one r2))]))

;; testing of Lem's procedures
(newline)

(par1
  (make-center-percent 1.0 0.1)
  (make-center-percent 3.0 0.1))

(par2
  (make-center-percent 1.0 0.1)
  (make-center-percent 3.0 0.1))

(newline) (display "as we can see the results are not the same") (newline)

(define A (make-center-percent 1.0 0.01))
(define B (make-center-percent 2.0 0.02))

(display "A: ") (display A) (newline)
(display "B: ") (display B) (newline)

(display "A/A: ") (display (center (div-interval A A))) (display " +- ") (display (percent (div-interval A A))) (display "%") (newline)
(display "A/B: ") (display (center (div-interval A B))) (display " +- ") (display (percent (div-interval A B))) (display "%") (newline)



