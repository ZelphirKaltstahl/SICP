#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

(provide (all-defined-out))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-binary-tree? x myset)
  (cond
    [(empty? myset) (false)]
    [(= x (entry myset)) true]
    [(< x (entry myset)) (element-of-set-binary-tree? x (left-branch myset))]
    [(> x (entry myset)) (element-of-set-binary-tree? x (right-branch myset))]))

(define (adjoin-set x myset)
  (cond
    [(empty? myset) (make-tree x nil nil)]
    [(= x (entry myset)) myset]
    [(< x (entry myset))
      (make-tree
        (entry myset)
        (adjoin-set x (left-branch myset))
        (right-branch myset))]
    [(> x (entry myset))
      (make-tree
        (entry myset)
        (left-branch myset)
        (adjoin-set x (right-branch myset)))]))

(define (tree->list-1 tree)
  (if
    (empty? tree)
    nil
    (append
      (tree->list-1 (left-branch tree))
      (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if
      (empty? tree)
      result-list
      (copy-to-list
        (left-branch tree)
        (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree nil))

;; tree 1
(display (tree->list-1
  (make-tree 7
    (make-tree 3
      (make-tree 1 nil nil)
      (make-tree 5 nil nil))
    (make-tree 9
      nil
      (make-tree 11 nil nil)))))

(newline)
(display (tree->list-2
  (make-tree 7
    (make-tree 3
      (make-tree 1 nil nil)
      (make-tree 5 nil nil))
    (make-tree 9
      nil
      (make-tree 11 nil nil)))))
(newline)
(newline)

;; tree 2
(display (tree->list-1
  (make-tree 3
    (make-tree 1 nil nil)
    (make-tree 7
      (make-tree 5 nil nil)
      (make-tree 9
        nil
        (make-tree 11 nil nil))))))
(newline)

(display (tree->list-2
  (make-tree 3
    (make-tree 1 nil nil)
    (make-tree 7
      (make-tree 5 nil nil)
      (make-tree 9
        nil
        (make-tree 11 nil nil))))))
(newline)
(newline)

;; tree 3
(display (tree->list-1
  (make-tree 5
    (make-tree 3
      (make-tree 1 nil nil)
      nil)
    (make-tree 9
      (make-tree 7 nil nil)
      (make-tree 11 nil nil)))))
(newline)

(display (tree->list-2
  (make-tree 5
    (make-tree 3
      (make-tree 1 nil nil)
      nil)
    (make-tree 9
      (make-tree 7 nil nil)
      (make-tree 11 nil nil)))))
(newline)
(newline)

; they always produce the same lists
; procedure 1 uses append, so the longer the list gets, the more elements append will go through to reach the end of the list. procedure 2 does not use append and only uses recursion and cons, so it will grow slower than procedure 1



