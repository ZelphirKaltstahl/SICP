#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; === GIVEN CODE ===
; note: I made the code more readable by actually using meaningful variable names.
(define (queens board-size)
  (define (solve col-count)
    (display "board-size is: ") (display col-count) (newline)
    (display "working on col: ") (display col-count) (newline)
    (if
      ; If the board is of size 0 ...
      (= col-count 0)
      ; ... we return the list, which contains the empty board as the only solution.
      (begin
        (display "board-size is 0") (newline)
        (list empty-board))
      ; Otherwise we filter ...
      (filter
        ; ... with the safe? predicate to obtain only solutions, where queens do not attack each other, ...
        ; (note: one "queen-positions" is actually one "board-position")
        ; (We want only valid board-positions!!! So we have to filter the set of all possible board-positions.)
        (λ (queen-positions)
          (display "checking for queen safe to add.") (newline)
          (display "current queen-positions: ") (display queen-positions) (newline)
          (display "trying to add queen at: ") (display col-count) (newline)
          (safe? col-count queen-positions))
        ; ... a list of potential solutions,
        ; which is the result of an application of flatmap.
        ; This should contain all possible board-positions.
        (flatmap
          ; procedure, which we apply to all solutions of baord size k-1
          ; board-position is one solution for k-1 queens or a board size of k-1,
          ; while being in the k-th iteration
          (λ (board-position)
            (display "current board-position: ") (display board-position) (newline)
            (display "getting map: ") (display (map (λ (new-row) (add-queen board-position (make-square new-row col-count))) (enumerate-interval 1 board-size))) (newline)
            ; For each solution for a board of size k-1 ...
            (map
              ; ... add a queen to the board position in row new-row and column k ...
              (λ (new-row)
                (add-queen board-position (make-square
                  new-row
                  col-count)))
              ; ... for all possible row indices with the current board size.
              ; NOTE: THIS STARTS AT 1 !!!
              ; This means all other parts of the program need to do the same or this needs to change
              (enumerate-interval 1 board-size)))
          ; the solutions for a board size of k-1
          (solve (- col-count 1))))))
  
  ; We want the queen columns for a given board size!
  ; A queen column is actually simply a row index, in which the queen is positioned in that column.
  ; We only interpret it as a whole column.
  (solve board-size))
  
  
  (define (make-square row col)
  (list row col))

(define (get-square-row square)
  (car square))

(define (get-square-col square)
  (cadr square))
   (define empty-board '()) 
  
  
 (define (add-queen square rest-of-queens) 
   (cons square rest-of-queens)) 
  
  
 (define (safe? k positions) 
   (check-safe 
    (car positions) 
    1 
    (cdr positions))) 
  
  
 ;; Check the queens column position against that of all other columns 
 ;; One if vertical distance = horizontal distance it's a diagonal hit 
 (define (check-safe position distance cols) 
   (cond ((null? cols) #t) 
         ((= (car cols) position) #f) 
         ((= (- (car cols) distance) position) #f) 
         ((= (+ (car cols) distance) position) #f) 
         (else (check-safe position (+ distance 1) (cdr cols))))) 
  
 ; Helper procedures 
 (define (flatmap proc seq) 
   (accumulate append '() (map proc seq))) 
  
  
 (define (accumulate op init seq) 
   (if (null? seq) 
       init 
       (op (car seq) 
           (accumulate op init (cdr seq))))) 
  
  
 (define (enumerate-interval low high) 
   (if (> low high) 
       '() 
       (cons low (enumerate-interval (+ low 1) high)))) 
       
(queens 6)
  
