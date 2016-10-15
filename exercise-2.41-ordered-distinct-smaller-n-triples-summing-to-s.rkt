#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

;; PREDEFINED CODE
(define (filter predicate sequence)
  (cond
    [(null? sequence)
      nil]
    [(predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))]
    [else
      (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  ;; (1 2 3) --> (op 1 (op 2 (op 3 init)))
  ;; the sequence is folded from the right to the left,
  ;; the operation with the first element requires all previous results folded into the in between result
  (if
    (empty? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if
    (> low high)
    nil
    (cons
      low
      (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond
    [(empty? tree) nil]
    [(not (pair? tree)) (list tree)]
    [else (append
      (enumerate-tree (car tree))
      (enumerate-tree (cdr tree)))]))

(define (accumulate-n op init seqs)
  (if
    (empty? (car seqs))
    nil
    (cons
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  ;; (1 2 3) --> (op (op (op init 1) 2) 3)
  ;; the sequence is folded from the left to the right,
  ;; the operation with the last element requires all previous results folded into the in between result
  (define (iter result rest)
    (if
      (empty? rest)
      result
      (iter
        (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; EXERCISE 2.41
;; i < j < k <= n with i+j+k = s triples

(define (unique-triples minimum maximum)
  (accumulate
    append
    nil
    (flatmap                                        ; for all i
      (λ (i)                                        ; given i
        (map                                        ; for all j
          (λ (j)                                    ; given j
            (map                                    ; for all k
              (λ (k) (list i j k))                  ; build list (i j k)
              (enumerate-interval minimum (- j 1))  ; make k values, always smaller than j values
            )
          )
          (enumerate-interval minimum (- i 1))      ; make j values, always smaller than i values
        )
      )
      (enumerate-interval minimum maximum)          ; make i values
    )
  )
)

(define (triple-sum triple)
  (+ (car triple) (cadr triple) (caddr triple)))

(define (get-triple-checksum-predicate checksum)
  (λ (triple)
    (= (triple-sum triple) checksum)))

(define (exercise-triples s n)
  (filter (get-triple-checksum-predicate s) (unique-triples 1 n)))

(exercise-triples 20 10)

;; UNIT TESTS
(define (check-equal?-with-output a b failure-msg)
  (display "checking for equality:") (newline)
  (display a) (newline)
  (display b) (newline)
  (check-equal? a b failure-msg))

(define (run-test-newlines a-test-suite)
  (for-each
    (λ (elem)
      (display elem) (newline))
    (run-test a-test-suite)))

(define exercise-test
  (test-suite
    "exercise test"
    #:before (λ () (display "before") (newline))
    #:after (λ () (display "after") (newline))
    
    (test-case
      "test case for triple sum"
      (check-equal?
        (triple-sum (list 1 2 3))
        6
        "triple-sum is not working correctly")
      (check-equal?
        (triple-sum (list 1 3 0))
        4
        "triple-sum is not working correctly")
      (check-equal?
        (triple-sum (list 1 10 342))
        353
        "triple-sum is not working correctly")
      (check-equal?
        (triple-sum (list 42 42 42))
        126
        "triple-sum is not working correctly"))
    
    (test-case
      "test case for get-triple-checksum-predicate predicate"
      (check-equal?
        ((get-triple-checksum-predicate 6) (list 1 2 3))
        #t
        "triple-checksum? predicate does not work correctly")
      (check-equal?
        ((get-triple-checksum-predicate 6) (list 99 2 3))
        #f
        "triple-checksum? predicate does not work correctly")
      (check-equal?
        ((get-triple-checksum-predicate 56) (list 20 17 19))
        #t
        "triple-checksum? predicate does not work correctly"))
    
    (test-case
      "test case for unique-triples"
      (check-equal?-with-output
        (unique-triples 1 5)
        (list
          (list 3 2 1)  ; 5 4 missing
          (list 4 2 1)  ; 5 3 missing
          (list 4 3 1)  ; 5 2 missing
          (list 4 3 2)  ; 5 1 missing
          (list 5 2 1)  ; 4 3 missing
          (list 5 3 1)  ; 4 2 missing
          (list 5 3 2)  ; 4 1 missing
          (list 5 4 1)  ; 3 2 missing
          (list 5 4 2)  ; 3 1 missing
          (list 5 4 3))  ; 2 1 missing
        "unique-triples does not return correct triples"))
  ))

(run-test-newlines exercise-test)
