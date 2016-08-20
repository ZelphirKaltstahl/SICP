#lang racket

(define (calculate-cell row col)
	(cond
		((< row 0) 0)
		((and (= row 0) (= col 0)) 1)
		((and (= row 0) (not (= col 0))) 0)
		((> row 0) (+
			(calculate-cell (- row 1) (- col 1))
			(calculate-cell (- row 1) col)))))

(define (calculate-row row col)
	(cond
		((= col row) (list (calculate-cell row col)))
		(else (cons (calculate-cell row col) (calculate-row row (+ col 1))))))


(define (yht row-count counter)
	(if 
		(= counter row-count)
		(print (calculate-row (- row-count counter) 0))
		(begin
			(yht row-count (+ counter 1))
			(print (calculate-row (- row-count counter) 0)))))

(define (yang-hui-triangle row-num)
	(yht row-num 0))

(yang-hui-triangle 7)