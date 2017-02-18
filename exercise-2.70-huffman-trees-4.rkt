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

;; creates a sorted list of nodes by inserting an element at the right place
;; (insertion sort like)
;; This is stable. Means that first element with a specific weight value will count as lower than the second element with the same weight value. "First means first."
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
               (adjoin-set (make-leaf (car pair) (cdr pair))
                           (make-leaf-set (rest pairs))))]))

;; The behavior of cons is different from append.
;; append appends elements of a list to another list.
;; cons appends lists to a list of element, which results in a list of lists.
;; One would need another procedure (flatten) to get the correct result.
(define (encode message tree)
  (define (iter message tree result)
    (cond
      [(empty? message) (reverse result)]
      [else (iter (rest message)
                  tree
                  (cons (encode-symbol (car message) tree)
                        result))]))
  (flatten (iter message tree nil)))

;; This is the encode procedure from the book.
;(define (encode message tree)
;  (if (null? message)
;    nil
;    (append (encode-symbol (first message) tree)
;    (encode (rest message) tree))))

(define (encode-symbol a-symbol huffman-tree)
  (define (encode-iter a-symbol subtree path)
    (cond
      ;; If the subtree is a leaf, compare the symbols.
      ;; If the symbols are equal, return the path taken so far.
      [(leaf? subtree)
        (if (eq? (symbol-leaf subtree) a-symbol)
          (reverse path)
          false)]
      ;; Each symbol can only be once encoded in the tree.
      ;; A path is unique amongst the encodings of symbols.
      ;; Since we return false when a leaf does not contain the symbol we searched for,
      ;; the or expressions will all but one return false.
      ;; Only for the correct path we will get a list of bits.
      ;; That is the list we will return.
      ;; However, this produces 2 procedure calls for each recursion step.
      ;; Evaluation is depth first so we will have height of tree elements in form of procedure calls in memory. We reach a leaf first, then one procedure's context can be taken from the stack, but we enter another branch down to a leaf again.
      [else
        (or
          (encode-iter a-symbol
                       (left-branch subtree) ; need to check this
                       (cons 0 path))
          (encode-iter a-symbol
                       (right-branch subtree)
                       (cons 1 path)))]))
  ;; at the beginning the whole tree and an empty list is given as a result
  (let
    ([encoded-message (encode-iter a-symbol huffman-tree nil)])
    (if encoded-message
        encoded-message
        (error "Symbol not found in tree." a-symbol))))

;; given in exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; TASK: write successive-merge
;; The procedure shall combine subtrees with the lowest frequency according to Huffman's algorithm.
(define (successive-merge ordered-nodes-set)
  (cond
    ;; For an empty set of nodes, we return the empty set.
    [(empty? ordered-nodes-set) nil]
    ;; If there are less than 2 (1) elements in the set of nodes to merge, we are done.
    [(< (length ordered-nodes-set) 2) (first ordered-nodes-set)]
    ;; Otherwise merge the first two elements into a subtree, since they are the ones with lowest weight.
    [else
     (let*
       ([new-node
          (combine-subtrees
            ;; Invert second and first here,
            ;; if you want symbols with high weight to take mostly paths of zero,
            ;; instead of paths of one.
            (first ordered-nodes-set)
            (second ordered-nodes-set))]
        [updated-ordered-nodes-set (adjoin-set new-node
                                               (cddr ordered-nodes-set))])
       (successive-merge updated-ordered-nodes-set))]))

;; TASK: encode
;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom

;; (define (unique-counts a-symbol-list)
;;   ;; counts should be a vector, for O(1) access
;;   (define (iter sublist counts)
;;     (cond
;;       ;; if the sublist is empty, return the counts
;;       [(empty? sublist) counts]
;;       ;; otherwise increase the count for the current element
;;       [else
;;         (iter
;;           (rest sublist)
;;           (increase-count-for-elem counts (first sublist)))]))
;;   (counts-vector-to-list-of-pairs (iter a-symbol-list (vector))))1

;; TODO: simplify into one let*
(let*
  ([message
     (string-replace
       (string-append
         "Get a job\n"
         "Sha na na na na na na na na\n"
         "Get a job\n"
         "Sha na na na na na na na na\n"
         "Wah yip yip yip yip yip yip yip yip yip\n"
         "Sha boom") "\n" " \n ")]
    [symbols-message (map string->symbol (string-split (string-upcase message) " "))]
    ;; this list of count pairs would have to be calculated by the programm actually ...
    [huffman-tree (generate-huffman-tree (list
                                           (cons 'BOOM 1)
                                           (cons 'WAH 1)
                                           (cons 'A 2)
                                           (cons 'GET 2)
                                           (cons 'JOB 2)
                                           (cons 'SHA 3)
                                           (cons (string->symbol "\n") 5)
                                           (cons 'YIP 9)
                                           (cons 'NA 16)))])
  (display huffman-tree) (newline)
  (display (encode symbols-message huffman-tree)) (newline)
  (display (length (encode symbols-message huffman-tree))) (newline))

