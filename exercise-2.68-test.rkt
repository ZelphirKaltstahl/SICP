#lang racket

(require rackunit
         "exercise-2.68-huffman-trees-2.rkt")

(test-case
  "test case for encoding messages"

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
       [sample-message
         (list 'A
               'A
               'B
               'A
               'B
               'C
               'D
               'A
               'C
               'D
               'B)])
      (encode sample-message sample-tree))
    (list 0
          0
          1 1 0
          0
          1 1 0
          1 0
          1 1 1
          0
          1 0
          1 1 1
          1 1 0)
    "not working correctly")

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
       [sample-symbol 'A])
      (encode-symbol sample-symbol sample-tree))
    (list 0)
    "not working correctly")

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
       [sample-symbol 'B])
      (encode-symbol sample-symbol sample-tree))
    (list 1 1 0)
    "not working correctly")

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
       [sample-symbol 'C])
      (encode-symbol sample-symbol sample-tree))
    (list 1 0)
    "not working correctly")
  
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
       [sample-symbol 'D])
      (encode-symbol sample-symbol sample-tree))
    (list 1 1 1)
    "not working correctly"))
