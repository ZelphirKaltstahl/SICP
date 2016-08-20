#lang racket

;; needed for compatibility reasons
(#%require (only racket/base current-inexact-milliseconds))
(define (runtime) (current-inexact-milliseconds))

;; basic function
(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (even? n) (divides? 2 n))

;; exact prime number test
(define (next n)
  (if
    (= n 2)
    3
    (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

;; unmodified version, using (+ test-divisor 1)
;(define (find-divisor n test-divisor)
;  (cond
;    ((> (square test-divisor) n) n)
;    ((divides? test-divisor n) test-divisor)
;    (else (find-divisor n (+ test-divisor 1)))))

;; modified version, using (next test-divisor)
(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

;; timed prime test
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))  ; takes time before function call

(define (start-prime-test n start-time)
  (if
    (prime? n)
    (report-prime (- (runtime) start-time))  ; calculates passed time since before function call
    (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; test code
;(timed-prime-test 1009)
;(timed-prime-test 1013)
;(timed-prime-test 1019)

;(timed-prime-test 10007)
;(timed-prime-test 10009)
;(timed-prime-test 10037)

;(timed-prime-test 100003)
;(timed-prime-test 100019)
;(timed-prime-test 100043)

;(timed-prime-test 1000003)
;(timed-prime-test 1000033)
;(timed-prime-test 1000037)

(timed-prime-test 100000000057)

;; unmodified
; 1009 *** 0.12109375         ; first result should be ignored for initialization reasons and garbage collection
; 1013 *** 0.004150390625     ;
; 1019 *** 0.005126953125     ;
; 10007 *** 0.01220703125     ;
; 10009 *** 0.011962890625    ;
; 10037 *** 0.012939453125    ;
; 100003 *** 0.036865234375   ;
; 100019 *** 0.035888671875   ;
; 100043 *** 0.037109375      ;
; 1000003 *** 0.11376953125   ;
; 1000033 *** 0.114013671875  ;
; 1000037 *** 0.114990234375  ;

;; modified
; 1009 *** 0.126953125       ; first result should be ignored for initialization reasons and garbage collection
; 1013 *** 0.0029296875      ; ~ 3/4
; 1019 *** 0.00390625        ; ~ 2/5
; 10007 *** 0.009033203125   ; ~ 3/4
; 10009 *** 0.009033203125   ; ~ 3/4
; 10037 *** 0.009033203125   ; ~ 3/4
; 100003 *** 0.026123046875  ; ~ 2/3
; 100019 *** 0.026123046875  ; ~ 2/3
; 100043 *** 0.02490234375   ; ~ 2/3
; 1000003 *** 0.0810546875   ; ~ 2/3
; 1000033 *** 0.0810546875   ; ~ 2/3
; 1000037 *** 0.08203125     ; ~ 2/3

; The timings do not really reflect half the amount of time needed for calculation.
; Maybe we need to add an example, which has higher runtime, so that the noise has less impact.


;; unmodified
; 1000000033 *** 3.79296875
;; modified
; 1000000033 *** 2.239990234375

; Still not really half the time.

;100000000057 *** 39.06103515625
;100000000057 *** 23.58984375

; Fraction: ~ 3/5

; Explanation:

; There is probably overhead for function calls,
; which is not yet insignificant and
; garbage collection can affect the results.