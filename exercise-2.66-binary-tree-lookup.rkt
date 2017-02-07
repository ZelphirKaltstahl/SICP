#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(provide (all-defined-out))

;; gets the entry along a tree
;; (the root of the subbranches)
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

;; creates a tree, given its right and left branch
(define (make-tree entry left right)
  (list entry left right))

;; checks whether an element is in a binary tree or not
(define (element-of-set-binary-tree? x myset)
  (cond
    [(empty? myset) (false)]
    [(= x (entry myset)) true]
    [(< x (entry myset)) (element-of-set-binary-tree? x (left-branch myset))]
    [(> x (entry myset)) (element-of-set-binary-tree? x (right-branch myset))]))

;; adds an element to a binary tree
(define (adjoin-set x myset)
  (cond
    ;; if we do not have any elements yet, simply make a new tree with the given x
    [(empty? myset) (make-tree x nil nil)]
    ;; if we find the element x in the tree, we do not need to add it again (set logic)
    [(= x (entry myset)) myset]
    ;; if x is smaller than the current root of subbranches
    ;; we make a new tree with the x inserted somewhere in the left subbranch
    ;; this might eventually lead to an empty subtree, where we can simply insert the element or
    ;; to a root of subbranches, which is equal to the element x, in which case we do not add it at all
    [(< x (entry myset))
      (make-tree
        (entry myset)
        (adjoin-set x (left-branch myset))
        (right-branch myset))]
    ;; same as for the < case but for >
    [(> x (entry myset))
      (make-tree
        (entry myset)
        (left-branch myset)
        (adjoin-set x (right-branch myset)))]))

;; NEW IN EXERCISE 2.66
;; EXAMPLE CODE
;; for sets kept as unordered lists
(define (lookup given-key set-of-records)
  (cond
    [(empty? set-of-records) false]
    [(equal? given-key (key (car set-of-records)))]
    [else (lookup given-key (cdr set-of-records))]))

;; MY CODE

;; model a record as a list of two strings and a key
(define (make-record key str1 str2)
  (list key str1 str2))

(define (key a-record)
  (first a-record))

;; procedure for finding a record given a key in a binary tree
(define (lookup-binary-tree given-key bin-tree-set-of-records)
  (cond
    [(empty? bin-tree-set-of-records)
       false]
    [(equal? given-key (key (entry bin-tree-set-of-records)))
       (entry bin-tree-set-of-records)]
    [(< given-key (key (entry bin-tree-set-of-records)))
       (lookup-binary-tree given-key (left-branch bin-tree-set-of-records))]
    [(> given-key (key (entry bin-tree-set-of-records)))
       (lookup-binary-tree given-key (right-branch bin-tree-set-of-records))]
    [else
      (display "strange case") (newline)]))


(display (make-tree (make-record 2 "D" "D")
                    (make-tree (make-record 4 "E" "E")
                               (make-record 3 "A" "A")
                               (make-record 5 "B" "B"))
                    (make-record 1 "C" "C")))



