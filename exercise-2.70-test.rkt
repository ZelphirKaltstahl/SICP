#lang racket

(require rackunit
         "exercise-2.70-huffman-trees-4.rkt")

(test-case
  "test case for successive-merge"

  (check-equal?
    (successive-merge (list (make-leaf 'BOOM 1)
                            (make-leaf 'WAH 1)
                            (make-leaf 'A 2)
                            (make-leaf 'GET 2)
                            (make-leaf 'JOB 2)
                            (make-leaf 'SHA 3)
                            (make-leaf 'YIP 9)
                            (make-leaf 'NA 16)))
    '()

    "not working correctly"))
