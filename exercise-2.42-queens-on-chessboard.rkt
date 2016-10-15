#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; PREDEFINED CODE
(define (filter predicate sequence)
  (cond
    [(empty? sequence)
      nil]
    [(predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))]
    [else
      (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  ;; (1 2 3) --> (op 1 (op 2 (op 3 init)))
  ;; the sequence is folded from the right to the left,
  ;; the operation with the first element requires all previous results folded into the in between result
  (if
    (empty? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if
    (> low high)
    nil
    (cons
      low
      (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond
    [(empty? tree) nil]
    [(not (pair? tree)) (list tree)]
    [else (append
      (enumerate-tree (car tree))
      (enumerate-tree (cdr tree)))]))

(define (accumulate-n op init seqs)
  (if
    (empty? (car seqs))
    nil
    (cons
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  ;; (1 2 3) --> (op (op (op init 1) 2) 3)
  ;; the sequence is folded from the left to the right,
  ;; the operation with the last element requires all previous results folded into the in between result
  (define (iter result rest)
    (if
      (empty? rest)
      result
      (iter
        (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; EXERCISE 2.42
;; chessboard queens problem

;; === GIVEN CODE ===
; note: I made the code more readable by actually using meaningful variable names.
(define (queens board-size)
  (define (queen-cols col-count)
    (display "working on col ") (display col-count) (newline)
    (if
      ; If the board is of size 0 ...
      (= col-count 0)
      ; ... we return the list, which contains the empty board as the only solution.
      (list empty-board)
      ; Otherwise we filter ...
      (filter
        ; ... with the safe? predicate to obtain only solutions, where queens do not attack each other, ...
        ; (note: one "queen-positions" is actually one "board-position")
        ; (We want only valid board-positions!!! So we have to filter the set of all possible board-positions.)
        (λ (queen-positions) (safe? col-count queen-positions))
        ; ... a list of potential solutions,
        ; which is the result of an application of flatmap.
        ; This should contain all possible board-positions.
        (flatmap
          ; procedure, which we apply to all solutions of baord size k-1
          ; rest-of-queens is one solution for k-1 queens or a board size of k-1,
          ; while being in the k-th iteration
          (λ (rest-of-queens)
            ; For each solution for a board of size k-1 ...
            (map
              ; ... add a queen to the board position in row new-row and column k ...
              (λ (new-row)
                (adjoin-position
                  new-row
                  col-count
                  rest-of-queens))
              ; ... for all possible row indices with the current board size.
              (enumerate-interval 1 board-size)))
          ; the solutions for a board size of k-1
          (queen-cols (- col-count 1))))))
  
  ; We want the queen columns for a given board size!
  ; A queen column is actually simply a row index, in which the queen is positioned in that column.
  ; We only interpret it as a whole column.
  (queen-cols board-size))

;; === Explanation ===
; rest-of-queens:
;   solution for k-1 queens
; new-row:
;   proposed placement for an additional queen in the k-th column
; adjoin-positions:
;   joins a new-row with an existing solution for k-1 columns
; empty-board:
;   represents an empty chessboard
; safe?:
;   predicate for determining if a new-row is safe for adjoining with an existing (safe) positioning of queens

;; === TODO ===
; * representation for sets of board positions
; * adjoin-position: adjoining a new row-column position to a set of queen positions
; * representation of an empty set of queen positions as empty-board
; * safe? determining for a set of queen positions if addition of a queen position is safe

(define (make-square row col)
  (list row col))

(define (get-square-row square)
  (car square))

(define (get-square-col square)
  (cadr square))

(define (get-nth-item sequence n)
  "this procedure uses zero indexing"
  (list-ref sequence n))

(define make-board (λ () nil))

(define empty-board nil)

(define (add-queen board square)
  (cond
    [(empty? board)
      (list square)]
    [else
      (append board (list square))]))

(define (adjoin-position row col current-board)
  (add-queen
    current-board
    (make-square row col)))

(define (on-same-diagonal? square1 square2)
  (=
    (abs (- (get-square-row square1) (get-square-row square2)))
    (abs (- (get-square-col square1) (get-square-col square2)))))

(define (on-same-row? square1 square2)
  (=
    (get-square-row square1)
    (get-square-row square2)))

(define (on-same-col? square1 square2)
  (=
    (get-square-col square1)
    (get-square-col square2)))

(define (find-max-column queens)
  (define (iter current-max remaining-queens)
    (cond
      [(empty? remaining-queens) current-max]
      [else (cond
        [(> (get-square-col (car remaining-queens)) current-max)
          (iter (get-square-col (car remaining-queens)) (cdr remaining-queens))]
        [else (iter current-max (cdr remaining-queens))])]))
  (iter -1 queens))


(define (get-in-check-predicate square1)
  (λ (square2)
    (or
      (on-same-row? square1 square2)
      (on-same-col? square1 square2)
      (on-same-diagonal? square1 square2))))

(define (safe? row board-position)
  (let
    [(square1 (make-square
      row
      (+ (find-max-column board-position) 1)))]
    [empty? (filter
      (get-in-check-predicate square1)
      board-position)]))

;; UNIT TESTS
(define (check-equal?-with-output a b failure-msg)
  (display "checking for equality:") (newline)
  (display a) (newline)
  (display b) (newline)
  (check-equal? a b failure-msg))

(define (run-test-newlines a-test-suite)
  (for-each
    (λ (elem)
      (display elem) (newline))
    (run-test a-test-suite)))

(define exercise-test
  (test-suite
    "exercise test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))
    
    (test-case
      "test case for make-square"
      (check-equal?
        (make-square 2 5)
        (list 2 5)
        "make-square does not work correctly"))
    
    (test-case
      "test case for get-square-row"
      (check-equal?
        (get-square-row (make-square 4 3))
        4
        "get-square-row does not work correctly"))
    
    (test-case
      "test case for get-square-col"
      (check-equal?
        (get-square-col (make-square 4 3))
        3
        "get-square-col does not work correctly"))
    
    (test-case
      "test for get-nth-item"
      (check-equal?
        (get-nth-item (list 2 3 4 5 6 7) 2)
        4
        "get-nth-item does not work correctly")
      (check-equal?
        (get-nth-item (list 232 4 345 2136 7) 3)
        2136
        "get-nth-item does not work correctly"))
    
    (test-case
      "test case for on-same-diagonal? predicate"
      (check-equal?
        (on-same-diagonal?
          (make-square 1 2)
          (make-square 4 5))
        #t
        "on-same-diagonal? predicate does not work correctly")
      (check-equal?
        (on-same-diagonal?
          (make-square 1 6)
          (make-square 1 5))
        #f
        "on-same-diagonal? predicate does not work correctly")
      (check-equal?
        (on-same-diagonal?
          (make-square 4 2)
          (make-square 7 3))
        #f
        "on-same-diagonal? predicate does not work correctly"))
    
    (test-case
      "test case for on-same-row? predicate"
      (check-equal?
        (on-same-row?
          (make-square 1 2)
          (make-square 4 5))
        #f
        "on-same-row? predicate does not work correctly")
      (check-equal?
        (on-same-row?
          (make-square 1 6)
          (make-square 1 5))
        #t
        "on-same-row? predicate does not work correctly")
      (check-equal?
        (on-same-row?
          (make-square 4 2)
          (make-square 7 3))
        #f
        "on-same-row? predicate does not work correctly"))
    
    (test-case
      "test case for on-same-col? predicate"
      (check-equal?
        (on-same-col?
          (make-square 1 2)
          (make-square 4 5))
        #f
        "on-same-col? predicate does not work correctly")
      (check-equal?
        (on-same-col?
          (make-square 1 6)
          (make-square 1 5))
        #f
        "on-same-col? predicate does not work correctly")
      (check-equal?
        (on-same-col?
          (make-square 4 3)
          (make-square 7 3))
        #t
        "on-same-col? predicate does not work correctly"))
    
    (test-case
      "test case for make-board"
      (check-equal? (make-board) nil
        "make-board does not work correctly"))
    
    (test-case
      "test case for add-queen"
      (check-equal?
        (add-queen
          (make-board)
          (make-square 1 2))
        (list (make-square 1 2))
        "add-queen does not work correctly")
      (check-equal?
        (add-queen
          (add-queen
            (make-board)
            (make-square 1 2))
          (make-square 3 4))
        (list (make-square 1 2) (make-square 3 4))
        "add-queen does not work correctly"))
    
    (test-case
      "test case for adjoin-position"
      (check-equal?
        (adjoin-position
          6 7
          (add-queen
            (add-queen
              (make-board)
              (make-square 1 2))
            (make-square 3 4)))
        (list (make-square 1 2) (make-square 3 4) (make-square 6 7))
        "adjoin-position failure"))
    
    (test-case
      "test case for safe? predicate"
      (check-equal?
        (safe?
          5
          (add-queen
            (add-queen
              (add-queen
                (add-queen
                  (make-board)
                  (make-square 0 0))
                (make-square 1 1))
              (make-square 2 2))
            (make-square 3 3)))
        #t
        "safe? failure")
      (check-equal?
        (safe?
          6
          (add-queen
            (add-queen
              (add-queen
                (add-queen
                  (make-board)
                  (make-square 0 1))
                (make-square 2 2))
              (make-square 3 0))
            (make-square 5 3)))
        #f
        "safe? failure")
      (check-equal?
        (safe?
          4
          (add-queen
            (add-queen
              (add-queen
                (make-board)
                (make-square 0 1))
              (make-square 2 2))
            (make-square 3 0)))
        #t
        "safe? failure"))
    
    (test-case
      "test case for find-max-column"
      (check-equal?
        (find-max-column
          (add-queen
            (add-queen
              (add-queen
                (add-queen
                  (make-board)
                  (make-square 0 0))
                (make-square 1 1))
              (make-square 2 2))
            (make-square 3 3)))
        3
        "find-max-column failure")
      (check-equal?
        (find-max-column
          (add-queen
            (add-queen
              (add-queen
                (make-board)
                (make-square 0 0))
              (make-square 1 1))
            (make-square 3 2)))
        2
        "find-max-column failure"))
    
    
  ))

(run-test-newlines exercise-test)

(display "Solutions to the queens problem with board-size 4 are: ") (display (queens 4)) (newline)

;;; OLD CODE

;(define (human-readable-col index)
;  (get-nth-item (list "A" "B" "C" "D" "E" "F" "G" "H") index))

;(define (human-readable-square square)
;  (list (human-readable-col (get-square-col square)) (+ (get-square-row square) 1)))

;(define (upper-border-square? board-size square)
;  (= (get-square-row square) 0))

;(define (right-border-square? board-size square)
;  (= (get-square-col square) (- board-size 1)))

;(define (lower-border-square? board-size square)
;  (= (get-square-row square) (- board-size 1)))

;(define (left-border-square? board-size square)
;  (= (get-square-col square) 0))

;(define (border-square? board-size square)
;  (or
;    (upper-border-square? board-size square)
;    (right-border-square? board-size square)
;    (lower-border-square? board-size square)
;    (left-border-square? board-size square)))

;    (test-case
;      "test case for human-readable-col"
;      (check-equal?
;        (human-readable-col 4)
;        "E"
;        "human-readable-col does not work correctly")
;      (check-equal?
;        (human-readable-col 3)
;        "D"
;        "human-readable-col does not work correctly")
;      (check-equal?
;        (human-readable-col 1)
;        "B"
;        "human-readable-col does not work correctly"))
    
;    (test-case
;      "test case for human-readable-square"
;      (check-equal?
;        (human-readable-square (list 1 0))
;        (list "A" 2)
;        "human-readable-square does not work correctly")
;      (check-equal?
;        (human-readable-square (list 4 2))
;        (list "C" 5)
;        "human-readable-square does not work correctly")
;      (check-equal?
;        (human-readable-square (list 5 3))
;        (list "D" 6)
;        "human-readable-square does not work correctly")
;      (check-equal?
;        (human-readable-square (list 6 7))
;        (list "H" 7)
;        "human-readable-square does not work correctly"))

;(define (make-board board-size)
;  (define (make-row result counter)
;    (cond
;      [(< counter board-size)
;        (make-row (cons #f result) (+ counter 1))]
;      [else
;        result]))
;  (define (iter result counter)
;    (cond
;      [(< counter board-size)
;        (iter (cons (make-row nil 0) result) (+ counter 1))]
;      [else
;        result]))
;  (iter nil 0))

;(define (make-board board-size)
;  (define (make-row result counter) nil)
;  (define (iter result counter)
;    (cond
;      [(< counter board-size)
;        (iter (cons (make-row nil 0) result) (+ counter 1))]
;      [else result]))
;  (iter nil 0))
