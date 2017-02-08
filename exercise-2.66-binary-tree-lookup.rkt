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
;(define (adjoin-set x myset)
;  (cond
;    ;; if we do not have any elements yet, simply make a new tree with the given x
;    [(empty? myset) (make-tree x nil nil)]
;    ;; if we find the element x in the tree, we do not need to add it again (set logic)
;    [(= x (entry myset)) myset]
;    ;; if x is smaller than the current root of subbranches
;    ;; we make a new tree with the x inserted somewhere in the left subbranch
;    ;; this might eventually lead to an empty subtree, where we can simply insert the element or
;    ;; to a root of subbranches, which is equal to the element x, in which case we do not add it at all
;    [(< x (entry myset))
;      (make-tree
;        (entry myset)
;        (adjoin-set x (left-branch myset))
;        (right-branch myset))]
;    ;; same as for the < case but for >
;    [(> x (entry myset))
;      (make-tree
;        (entry myset)
;        (left-branch myset)
;        (adjoin-set x (right-branch myset)))]))

;; NEW IN EXERCISE 2.66
;; EXAMPLE CODE
;; for sets kept as unordered lists
;(define (lookup given-key set-of-records)
;  (cond
;    [(empty? set-of-records) false]
;    [(equal? given-key (key (car set-of-records)))]
;    [else (lookup given-key (cdr set-of-records))]))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))  ; calculating the length of a list is in O(n)

(define (partial-tree elements total-size)
  (if
    (= total-size 0)
    (cons nil elements)
    (let
      [(left-size (quotient (- total-size 1) 2))]
      [let
        [(left-result (partial-tree elements left-size))]
        [let
          [(left-tree (car left-result))
          (non-left-elements (cdr left-result))
          (right-size (- total-size (+ left-size 1)))]
          [let
            [(this-entry (car non-left-elements))
            (right-result (partial-tree (cdr non-left-elements) right-size))]
            [let
              [(right-tree (car right-result))
              (remaining-elements (cdr right-result))]
              [cons (make-tree this-entry left-tree right-tree) remaining-elements]]]]])))


;; NEW IN EXERCISE 2.66

;; model a record as a list of two strings and a key
(define (make-record key str1 str2)
  (list key (list str1 str2)))

(define (key a-record)
  (first a-record))

(define (record-data a-record)
  (rest a-record))

;; procedure for finding a record given a key in a binary tree
(define (lookup-binary-tree given-key bin-tree-set-of-records)
  (display "LOOKING UP ") (display given-key) (newline)
  (if (not (empty? bin-tree-set-of-records))
      (begin (display "TREE IS ") (display bin-tree-set-of-records) (newline)
             (display "ENTRY IS ") (display (entry bin-tree-set-of-records)) (newline)
             (display "ENTRY KEY IS ") (display (key (entry bin-tree-set-of-records))) (newline)
             (display "LEFT IS ") (display (left-branch bin-tree-set-of-records)) (newline)
             (display "RIGHT IS ") (display (right-branch bin-tree-set-of-records)) (newline))
      (begin (display "TREE WAS EMPTY") (newline)))

  (cond
    [(empty? bin-tree-set-of-records)
       (display "TREE WAS EMPTY. return false") (newline)
       false]
    [(= given-key (key (entry bin-tree-set-of-records)))
       (display (string-append "SEARCHING FOR " (number->string given-key) " FOUND KEY")) (newline)
       (entry bin-tree-set-of-records)]
    [(< given-key (key (entry bin-tree-set-of-records)))
       (display (string-append "SEARCHING FOR " (number->string given-key) " given-key is smaller")) (newline)
       (lookup-binary-tree given-key (left-branch bin-tree-set-of-records))]
    [(> given-key (key (entry bin-tree-set-of-records)))
       (display (string-append "SEARCHING FOR " (number->string given-key) " given-key is greater")) (newline)
       (lookup-binary-tree given-key (right-branch bin-tree-set-of-records))]
    [else
      (display "strange case") (newline)]))
