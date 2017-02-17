#lang racket

(require rackunit
         "exercise-2.69-huffman-trees-3.rkt")

(test-case
  "test case for successive-merge"

  (check-equal?
    (successive-merge (list (make-leaf 'A 1)
                            (make-leaf 'C 2)
                            (make-leaf 'B 3)
                            (make-leaf 'D 10)))
    (combine-subtrees
      (combine-subtrees
        (make-leaf 'B 3)
        (combine-subtrees
         (make-leaf 'A 1)
         (make-leaf 'C 2)))
      (make-leaf 'D 10))

    "not working correctly")
  
  (check-equal?
    (successive-merge (list (make-leaf 'A 1)
                            (make-leaf 'C 2)
                            (make-leaf 'B 3)
                            (make-leaf 'D 5)))
    (combine-subtrees
      (make-leaf 'D 5)
      (combine-subtrees
        (make-leaf 'B 3)
        (combine-subtrees
         (make-leaf 'A 1)
         (make-leaf 'C 2))))

    "not working correctly"))