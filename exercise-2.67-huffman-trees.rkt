#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; a general print function
(define (printline elems #:sep [sep " "] #:end [end "\n"] #:element-converter [element-converter identity])
  (define (iter remaining-elements result-string)
    (cond
      [(empty? remaining-elements) (string-append result-string end)]
      [(empty? (rest remaining-elements))
        (iter (rest remaining-elements)
              (string-append result-string
                             (element-converter (first remaining-elements))))]
      [else
        (iter (rest remaining-elements)
              (string-append result-string
                             (element-converter (first remaining-elements))
                             sep))]))
  (cond
    [(empty? elems) (display end)]
    [(not (list? elems)) (display (string-append (element-converter elems) end))]
    [else (display (iter elems ""))]))

(provide (all-defined-out))

;; EXERCISE 2.67
;; GIVEN CODE
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (second x))
(define (weight-leaf x)
  (third x))

(define (left-branch tree)
  (first tree))
(define (right-branch tree)
  (second tree))

(define (symbols tree)
  (cond
    [(leaf? tree) (list (symbol-leaf tree))]
    [else (caddr tree)]))
(define (weight tree)
  (cond
    [(leaf? tree) (weight-leaf tree)]
    [else (cadddr tree)]))

(define (combine-subtrees left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit: CHOOSE-BRANCH" bit)]))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (cond
      [(empty? bits) nil]
      [else (let ([next-branch (choose-branch (first bits) current-branch)])
                 (cond
                   [(leaf? next-branch) (cons (symbol-leaf next-branch)
                                              (decode-1 (rest bits) tree))]
                   [else (decode-1 (rest bits) next-branch)]))]))
  (decode-1 bits tree))

;; creates a sorted list by inserting an element at the right place
;; (insertion sort like)
(define (adjoin-set x set)
  (cond
    [(empty? set) (list x)]
    [(< (weight x) (weight (first set))) (cons x set)]
    [else (cons (first set) (adjoin-set x (rest set)))]))

(define (make-leaf-set pairs)
  (cond
    ;; if there are no pairs, there are no leaves ...
    [(empty? pairs) nil]
    ;; otherwise get the first pair and insert it in the ordered list
    ;; created from the rest of the pairs (recursively)
    [else (let ([pair (first pairs)])
               (adjoin-set (make-leaf (first pair) (second pair))
                           (make-leaf-set (rest pairs))))]))

;; TASK
;: Define an encoding tree and a sample message.
;; Example given:
(define sample-tree
  (combine-subtrees (make-leaf 'A 4)
                    (combine-subtrees (make-leaf 'B 2)
                                      (combine-subtrees (make-leaf 'D 1)
                                                        (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree)) (newline)
