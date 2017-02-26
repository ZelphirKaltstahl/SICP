;; Exercise 2.72: Consider the encoding procedure that you
;; designed in Exercise 2.68. What is the order of growth in
;; the number of steps needed to encode a symbol? Be sure
;; to include the number of steps needed to search the sym-
;; bol list at each node encountered. To answer this question
;; in general is difficult. Consider the special case where the
;; relative frequencies of the n symbols are as described in Ex-
;; ercise 2.71, and give the order of growth (as a function of n)
;; of the number of steps needed to encode the most frequent
;; and least frequent symbols in the alphabet.

;; Here is the code again:
(define (encode message tree)
  (define (iter message tree result)
    (cond
      [(empty? message) (reverse result)]
      [else (iter (rest message)  ; goes through the whole list ==> n
                  tree
                  (cons (encode-symbol (car message) tree)  ; for each symbol we have to look it up in the tree ==> n * something for the tree
                        result))]))
  (flatten (iter message tree nil)))

(define (encode-symbol a-symbol huffman-tree)
  (define (encode-iter a-symbol subtree path)
    (cond
      [(leaf? subtree)
        (if (eq? (symbol-leaf subtree) a-symbol)
            (reverse path)
            false)]  ; if we arrive at a leaf, return the reversed path, reversing is in O(???)
      [else
        (or
          (encode-iter a-symbol
                       (left-branch subtree)
                       (cons 0 path))
          (encode-iter a-symbol
                       (right-branch subtree)
                       (cons 1 path)))]))
  (let
    ([encoded-symbol (encode-iter a-symbol huffman-tree nil)])  ; the symbol is encoded using the tree.
    (if encoded-symbol
        encoded-symbol
        (error "Symbol not found in tree." a-symbol))))

;; Thoughts about the time complexity
;; ==================================

;; For lookup of the complete message to encode, we iterate through the list of symbols and encode each symbol on its own.
;; This makes for a time complexity of O(n * steps needed for encode one symbol).

;; To encode one symbol we iterate through the binary Huffman tree.
;; If the tree were balanced, it would take log_2(n) at max to get to a leaf. (Average Case???)
;; However, the tree can be imbalanced.

;; The worst case is when the tree always expands to one side only.
;; In that case we need n-1 steps to get to the least frequent leaf.
;; If we encode a message consisting only of such least frequent symbols,
;; that are least frequent, using a tree created for another message,
;; we get n-1 * n = n^2 - n, which is O(n^2).
;; Such message could be a substring of a longer message.

;; The best case would be encoding a message of only the most frequent symbols.
;; Say the symbols have frequencies of 2^m (1 2 4 8 ...).
;; Combining subtrees then can never result in a higher summed frequency than the next low frequency of a symbol in the set of symbols.
;; This causes the tree to expand always in one direction only.
;; In such a case the lookup of the most frequent symbol in the tree will always finish in the first leaf.

;; (or bool bool) is a shortcut operator.
;; If we construct the tree in such a way that the first parameter for (or bool bool) is also the one with the higher frequency,
;; the lookup will thus only take one step, resulting in a lookup of O(1).
;; This would give us a total encoding time complexity of O(n*1) = O(n).

;; Conclusions
;; ===========

;; - the order of parameters to (or bool bool) is important
;; - in which order (left (0) or right (1) highest frequency is important
