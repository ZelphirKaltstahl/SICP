#lang racket

(require rackunit
         "exercise-2.67-huffman-trees.rkt")

(test-case
  "binary tree records set lookup"

  (check-equal?
    (let
      ([sample-tree
         (combine-subtrees
           (make-leaf 'A 4)
           (combine-subtrees
             (make-leaf 'C 3)
             (combine-subtrees
               (make-leaf 'B 2)
               (make-leaf 'D 2))))]
       ;; message: A B D B AA CCC D A
       [sample-message
         (list 0
               1 1 0
               1 1 1
               1 1 0
               0
               0
               1 0
               1 0
               1 0
               1 1 1
               0)])
      (decode sample-message sample-tree))
    (list 'A
          'B
          'D
          'B
          'A
          'A
          'C
          'C
          'C
          'D
          'A)
    "not working correctly")

  (check-equal?
    '()
    '()
    "not working correctly")

  (check-equal?
    '()
    '()
    "not working correctly"))
