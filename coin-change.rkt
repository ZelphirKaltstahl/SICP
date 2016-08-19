#lang racket
(require racket/trace)

(define (count-change amount)
	(cc amount 8))

;; the real calculation
(define (cc amount kinds-of-coins)
	(cond
		((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+
			(cc amount (- kinds-of-coins 1))
			(cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

;; returns value for one kind of coins
(define (first-denomination kind-of-coins)
	(cond
		; ((= kind-of-coins 1) 1)
		; ((= kind-of-coins 2) 5)
		((= kind-of-coins 1) 1)
		((= kind-of-coins 2) 2)
		((= kind-of-coins 3) 5)
		((= kind-of-coins 4) 10)
		((= kind-of-coins 5) 20)
		((= kind-of-coins 6) 50)
		((= kind-of-coins 7) 100)
		((= kind-of-coins 8) 200)))

; (define (first-denomination kind-of-coins)
; 	(cond
; 		((= kind-of-coins 1) 10)
; 		((= kind-of-coins 2) 20)
; 		((= kind-of-coins 3) 50)))

(trace cc)
(count-change 10)