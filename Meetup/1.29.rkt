#lang racket
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (even? n)
  (= (remainder n 2) 0))
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (simpsons-rule f a b n-terms)
  (define h (/ (- b a)
               n-terms))

  (define (yk k)
    (f (+ a (* k h))))

  (define (term k)
    (display (string-append "term " (number->string k) " is: "))
    (let ([result (* (cond
                       ; first term and last term do not have coefficient
                       [(or (= k 0) (= k n-terms)) 1]
                       [(even? k) 2]
                       [else 4])
                     (yk k))])
      (displayln result)
      result))

  (define (next k)
    (+ k 1))

  (* (/ h 3)
     (sum term 0 next n-terms)))

(simpsons-rule cube 0 1 20)
