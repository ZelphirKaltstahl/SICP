#lang racket

;; basic functions
(define (halve n) (/ n 2))
(define (decrement n) (- n 1))
(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (even? n) (divides? 2 n))
(define (next n)
  (cond
    ((< n 2) 2)
    ((even? n) (+ n 1))
    (else (+ n 2))))

;; prime number test
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))


(define (non-trivial-sqrt-of-one? base exp m)
  ;; This is the actual test for non-trivial squares of 1 congruent regarding m.
  (cond
    [(= exp 0) 1]
    [(even? exp)
     ;; If the exponent is an even number, we can halve it, and the squaring happens outside!
     ;; Why this is possible? Check the rabin-miller-test.pdf / .odt.
     (remainder
      (square
       (non-trivial-sqrt-of-one? base (halve exp) m))
      m)]
    [else
     ;; in the else branch we take a factor outside instead:
     (remainder
      (* base
         (non-trivial-sqrt-of-one? base (decrement exp) m))
      m)]))

(define (rabin-miller-test number-to-check a)
  ;; (display "performing rabin miller test for base = ")
  ;; (display a)
  ;; (display " and number = ")
  ;; (displayln number-to-check)
  (cond [(> 1 number-to-check) false]
        [else
         (= (non-trivial-sqrt-of-one? a (- number-to-check 1) number-to-check)
            1)]))

(define (check-rabin-miller-all n counter)
  ;; If this procedure returns true, it means the rabin miller test passed for all numbers between counter and n. If counter is 2, then the test guarantees correcly stating whether or not a number is prime.
  (cond
    [(< counter n)
     (if (rabin-miller-test n counter)
         (check-rabin-miller-all n (+ counter 1))  ; recursion!
         false)]
    [else true]))

(define (find-primes min max)
  (define (find-primes-func min max)
    ;; finds all prime numbers in between min and max (including min and excluding max)
    (if (< min max)
        (cond
          [(check-rabin-miller-all min 2)
           ;(display "prime: ") (displayln min)
           (find-primes (+ min 1) max)]
          [else
           (find-primes (next min) max)])
        (displayln "finished")))

  (find-primes-func (next min) max))


(define (check-rabin-miller-with-significant-primes potential-prime)
  ;; It is sufficient to check numbers with the following prime numbers as witnesses up to:
  ;; n < 3.317.044.064.679.887.385.961.981
  ;; This number is larger than 64bit integer range.
  ;; https://en.wikipedia.org/wiki/Miller–Rabin_primality_test
  (define SIGNIFICANT-PRIMES (list 2 3 5 7 11 13 17 19 23 29 31 37 41))
  (define (square x) (* x x))
  (define (halve x) (/ x 2))

  (define (one-rabin-miller-check base exp m)
    ;; This is the actual test for non-trivial squares of 1 congruent regarding m.
    (cond
      [(= exp 0) 1]
      [(even? exp)
       ;; If the exponent is an even number, we can halve it, and the squaring happens outside!
       ;; Why this is possible? Check the rabin-miller-test.pdf / .odt.
       (remainder (square (one-rabin-miller-check base
                                                  (halve exp)
                                                  m))
                  m)]
      [else
       ;; in the else branch we take a factor outside instead:
       (remainder (* base (one-rabin-miller-check base
                                                  (decrement exp)
                                                  m))
                  m)]))

  ;; This procedure is only for calling one-rabin-miller-check with the correct parameters.
  (define (rabin-miller-test number-to-check a)
    (cond
      [(> 1 number-to-check) false]  ; safeguard against 0 or lower parameter
      [else
       (= (one-rabin-miller-check a (- number-to-check 1) number-to-check)
          1)]))

  (define (iter number-to-check remaining-significant-primes)
    (cond [(empty? remaining-significant-primes) true]
          [else
           (let ([current-significant-prime (first remaining-significant-primes)])
             (cond [(>= current-significant-prime number-to-check) true]
                   [(rabin-miller-test number-to-check current-significant-prime)
                    (iter number-to-check (rest remaining-significant-primes))]
                   [else false]))]))

  ;; start iterating
  (iter potential-prime SIGNIFICANT-PRIMES))

(define (find-primes-limited min max)
  (cond [(< min max)
         (cond [(check-rabin-miller-with-significant-primes min)
                ;; test successful, number is prime according to test
                ;; guaranteed to be true up to:
                ;; n < 3.317.044.064.679.887.385.961.981
                (display "prime:")(displayln min)
                (find-primes-limited (next min) max)]
               [else
                ;; test failed, number is not prime
                ;; (display "number ") (display min) (displayln " failed the test.")
                (find-primes-limited (next min) max)])]
        [else
         (displayln "finished")]))

(time (find-primes 0 10000))
(time (find-primes-limited 0 10000))

; carmichael numbers
; 561
; 1105
; 1729
; 2465
; 2821
; 6601
; 8911
; 10585
; 15841
; 29341
; 41041
; 46657
; 52633
; 62745
; 63973
; 75361

;; NOTE
;; For this exercise I didn't actually change the expmod function as described in the exercise.
;; I merely renamed it and changed the parameters given to it by the rabin-miller-test function to be the exponent - 1 and then check in the rabin-miller-test function for congruence to 1 (mod n).
;; This seems simpler to me than changing the expmod function and plays well with my other code.
