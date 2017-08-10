#lang racket

;; Operation, type -> procedure
;; Dispatch table.
;;
(define *op-table* (make-hasheq))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))
