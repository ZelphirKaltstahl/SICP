#lang racket
(define (square x) (* x x))
(define (avg a b) (/ (+ a b) 2))

(define (improve-guess x guess)
  (avg (/ x guess) guess))

(define (epsilon-square-root x guess)
  (abs (- (square guess) x)))

(define (precision-sufficient x guess precision)
  (< (epsilon-square-root x guess) precision))

(define (precision-sufficient-two x guess precision)
  (<
   (abs (- guess (improve-guess x guess)))
   (abs (* guess precision))))

(define (sqrt-iter guess x precision)
  (if
   (precision-sufficient x guess precision)
   guess
   (sqrt-iter (improve-guess x guess) x precision)))

(define (sqrt-iter-two guess x precision)
  (if
   (precision-sufficient-two x guess precision)
   guess
   (sqrt-iter-two (improve-guess x guess) x precision)))


(define (cube x) (* x x x))

(define (epsilon-cube-root x guess)
  (abs (- x (cube guess))))

(define (cube-guess-improve x guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root guess x precision)
  (if
   (< (epsilon-cube-root x guess) precision)
   guess
   (cube-root (cube-guess-improve x guess) x precision)))


(sqrt-iter 2.6 7 0.000001)
(cube-root 2.6 7 0.000001)
