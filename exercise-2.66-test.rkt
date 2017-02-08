#lang racket

(require rackunit
         "exercise-2.66-binary-tree-lookup.rkt")

(test-case
  "binary tree records set lookup"

  (check-equal?
    (lookup-binary-tree 1 (list->tree (list
                                        (make-record 5 "E" "E")
                                        (make-record 3 "C" "C")
                                        (make-record 7 "G" "G")
                                        (make-record 2 "B" "B")
                                        (make-record 4 "D" "D")
                                        (make-record 6 "F" "F"))))
    false
    "not working correctly")

  (check-equal?
    (lookup-binary-tree 6 (list->tree (list
                                        (make-record 5 "E" "E")
                                        (make-record 3 "C" "C")
                                        (make-record 7 "G" "G")
                                        (make-record 2 "B" "B")
                                        (make-record 4 "D" "D")
                                        (make-record 6 "F" "F"))))
    (make-record 6 "F" "F")
    "not working correctly")

  (check-equal?
    (lookup-binary-tree 1 (list->tree (list
                                        (make-record 5 "E" "E")
                                        (make-record 3 "C" "C")
                                        (make-record 7 "G" "G")
                                        (make-record 2 "B" "B")
                                        (make-record 4 "D" "D")
                                        (make-record 6 "F" "F"))))
    (make-record 2 "B" "B")
    "not working correctly"))
