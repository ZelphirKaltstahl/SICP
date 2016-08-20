#lang racket
(define (cube x) (* x x x))
(define (p x)
  (display 1)
  (newline)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if
    (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

; 5 times

; p is called as long as the angle is > 0.1 and each time we devide the angle by 3.
; This means we search the number of times, we devide the angle by 3. This is log_3(angle).
; So the number of steps is O(log_3(angle)).

; Space grows logarithmically as well because the recursion is linear and
; we only have log_3(angle) steps, in which each time an application of p needs to be remembered.
; p only gets called if the angle is still > 0.1 so only log_3(angle) applications of p need
; to be remembered.