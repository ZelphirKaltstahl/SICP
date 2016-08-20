#lang racket

;; Answers

; The interpreter calculates the result of the expmod call twice for each squaring,
; instead of applying the square function to the once calculated result of expmod.
; expmod is our recursive function.
; This means we will enter two branches or function calls with the code of Louis Reasoner.
; Every time an exponent is even, we will calculate using two recursive calls.
; Every time those sub calculations have an even exponent, they will also cause two function calls.
; Instead of halving the number of multiplications by halving the exponent,
; Louis is spawning twice the number of calculations with halved exponent.

; (But why does that make it "exactly" O(n) and not for example put a factor in front like O(a*log(n))?)
; (Which complexity are we talking about? Time or space?)