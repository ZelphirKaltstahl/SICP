#lang sicp
;(#%require sicp)
(#%require sicp-pict)

(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein2 einstein2))

(define (flipped-pairs painter)
  (let
    [(painter2 (beside painter (flip-vert painter)))]
    [below painter2 painter2]))

(define (right-split painter n)
  (if
    (= n 0)
    painter
    (let
      [(smaller (right-split painter (- n 2)))]
      [beside painter (below smaller smaller)])))

(right-split einstein 3)

;(define (corner-split painter n)
;  (if
;    (= n 0)
;    painter  ; the picture itself
;    (let
;      [(up (up-split painter (- n 1)))
;      (right (right-split painter (- n 1)))]
;      [let
;        [(top-left (beside up up))
;        (bottom-right (below right right))
;        (corner (corner-split painter (- n 1)))]
;        [beside
;          (below painter top-left)
;          (below bottom-right corner)]])))

;(define (square-limit painter n)
;  (let
;    [(quarter (corner-split painter n))]
;    [let
;      [(half (beside (flip-horiz quarter) quarter))]
;      [below (flip-vert halt) half]]))

;(right-split rogers 4)
