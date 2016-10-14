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


;; MILLER-RABIN-TEST CODE
;; basic functions
(define (halve n) (/ n 2))
(define (decrement n) (- n 1))
(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (even? n) (divides? 2 n))

(define (non-trivial-sqrt-of-one? base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
      (remainder (square (non-trivial-sqrt-of-one? base (halve exp) m)) m))
    (else
      (remainder (* base (non-trivial-sqrt-of-one? base (decrement exp) m)) m))))

(define (rabin-miller-test number-to-check a)
  (= (non-trivial-sqrt-of-one? a (- number-to-check 1) number-to-check) 1))

;; checks for all numbers smaller than the number we want to check for primality
(define (check-rabin-miller-all n counter)
  (if
    (< counter n)
    (if
      (rabin-miller-test n counter)
      (check-rabin-miller-all n (+ counter 1))
      false)
    true))

;; FLATMAP CODE
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; EXAMPLE CODE
;; 1. All 1 <= j <= i <= n so that j+i is prime
(define (unique-pairs minimum maximum)
  (accumulate
    append
    nil
    (map (lambda (i)
      (map (lambda (j)
        (list i j))
        (enumerate-interval minimum (- i 1))))
      (enumerate-interval minimum maximum))))

(define (prime-sum? pair)
  (define (probably-prime? n)
    (check-rabin-miller-all n 1))
  (probably-prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list
    (car pair)
    (cadr pair)
    (+ (car pair) (cadr pair))))

(define (print-list alist)
  (for-each
    (λ (elem) (display elem) (newline))
    alist))

(display "All 1 <= j <= i <= 20 so that j+i is prime:") (newline)
(print-list
  (map make-pair-sum (filter prime-sum? (unique-pairs 1 20))))
(newline)

;; 2. Permutations of lists
(define (permutations aset)
  (if
    (empty? aset)
    (list nil)
    (flatmap
      (lambda (elem)
        (map
          (lambda (subperm) (cons elem subperm))
          (permutations (remove elem aset))))
      aset)))

(display "All permutations of (1 2 3 4):") (newline)
(permutations (list 1 2 3 4))
(newline)

(display "All permutations of (1 2 2):") (newline)
(permutations (list 1 2 2))
(newline)

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
    
    
  ))

(run-test-newlines exercise-test)
