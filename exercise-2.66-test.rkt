#lang racket

(require rackunit
         "exercise-2.66-binary-tree-lookup.rkt")

(test-case
  "binary tree records set lookup"

  (check-equal?
    (lookup-binary-tree
      6
      (make-tree
        (make-record 2 "D" "D")
        (make-tree (make-record 4 "E" "E")
          (make-record 3 "A" "A")
          (make-record 5 "B" "B"))
        (make-record 1 "C" "C")))

    false

    "not working correctly")

  (check-equal?
    (lookup-binary-tree
      5
      (make-tree
        (make-record 2 "D" "D")
        (make-tree (make-record 4 "E" "E")
          (make-record 3 "A" "A")
          (make-record 5 "B" "B"))
        (make-record 1 "C" "C")))

    (make-record 5 "B" "B")

    "not working correctly")

  (check-equal?
    (lookup-binary-tree
      1
      (make-tree
        (make-record 2 "D" "D")
        (make-tree (make-record 4 "E" "E")
          (make-record 3 "A" "A")
          (make-record 5 "B" "B"))
        (make-record 1 "C" "C")))

    (make-record 1 "C" "C")

    "not working correctly"))
