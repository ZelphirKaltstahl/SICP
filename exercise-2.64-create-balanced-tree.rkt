#lang racket

(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

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

(define (list->tree elements)
  (car (partial-tree elements (length elements))))  ; calculating the length of a list is in O(n)

;; NEW IN EXERCISE 2.64
(define (partial-tree elements total-size)
  (if
    (= total-size 0)
    (cons nil elements)  ; If we don't have elements, the list will be the list, which contains only the empty list. (list nil)
    (let
      [(left-size (quotient (- total-size 1) 2))]
      ; the size of the left tree is:
      ; * the total size -1 for the highest element, which is the entry, which is neither right nor left
      ; * divided by 2, because we only want the left tree size and not also the right tree
      [let
        [(left-result (partial-tree elements left-size))]  ; recurse for left tree, reducing the problem size
        [let
          [(left-tree (car left-result))  ; in the car are the left tree elements in form of a tree (list left-tree non-left-tree-elements)
          (non-left-elements (cdr left-result))  ; in the cdr are the non-left-tree elements
          (right-size (- total-size (+ left-size 1)))]  ; the size of the right tree
          [let
            [(this-entry (car non-left-elements))  ; non left elements include the current entry
            (right-result (partial-tree (cdr non-left-elements) right-size))]  ; recurse for the right tree using the non left elements and the right size calculated previously
            [let
              [(right-tree (car right-result))  ; get right tree
              (remaining-elements (cdr right-result))]  ; get remaining elements
              [cons (make-tree this-entry left-tree right-tree) remaining-elements]]]]])))  ; finally build the tree completely and put the remaining elements in as well

;; HOW IT WORKS
;
; If there are no more elements to recurse on, because total-size is 0, 

(display (list->tree (list 1 3 5 7 9 11)))
