#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; HIGHEST ABSTRACTION LEVEL
;; We do not know anything about the underlying representation of the numbers.
;; Neither do we know, which specialized operations will be called at some point after dispatch.
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; Now we do a bottom-up approach by writing the code for the 4 basic operations for the specific representations.
;; PLAIN OLD ORDINARY NUMBERS
;; These procedures assume to be dealing with ordinary numbers.
;; They will be installed as a package.
;; That will register them in a table of (appropriate) operations for tagged data, which has been tagged as ordinary number.
(define (install-scheme-number-package)
  ;; define a tagging procedure for this package, so that all tagging in this package use the same tag
  ;; Using always the same tag reminds us that this package should only define procedures, which work on the type of data
  ;; which our package is supposed to support, according to its name.
  ;; It also shortens procedure calls, because the knowledge which tag shall be attached does not need to be supplied as a parameter,
  ;; because it is always the same throughout the whole package.
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
    ;; The result of operations of our package will be a number, which again is an ordinary number.
    ;; So we should tag it as such.
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))
  ;; We also define a way to create data, which is tagged as data our package understands how to handle.
  (put 'make 'scheme-number
    (lambda (x) (tag x)))
  ;; We return the symbol done, when we added all the operations to the table of operations.
  'done)

;; To access the make procedure of the package a user would use a specific procedure:
(define (make-scheme-number n)
  ;; get the appropriate operation from the table of operations
  ;; and apply it to n
  ((get 'make 'scheme-number) n))


;; RATIONAL NUMBERS
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let
      ([g (gcd n d)])
      (cons
        (/ n g)
        (/ d g))))
  (define (add-rat x y)
    (make-rat
      (+
        (*
          (numer x)
          (denom y))
        (*
          (numer y)
          (denom x)))
      (*
        (denom x)
        (denom y))))
  (define (sub-rat x y)
    (make-rat
      (-
        (*
          (numer x)
          (denom y))
        (*
          (numer y)
          (denom x)))
      (*
        (denom x)
        (denom y))))
  (define (mul-rat x y)
    (make-rat
      (*
        (numer x)
        (numer y))
      (*
        (denom x)
        (denom y))))
  (define (div-rat x y)
    (make-rat
      (*
        (numer x)
        (denom y))
      (*
        (denom x)
        (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


;; COMPLEX NUMBERS
;; In contrast to rational number and ordinary numbers complex numbers have two separate representations.
;; This means that this package will be using generic operations, which are generic regarding the fact that there are two representations for complex numbers, but specific to complex numbers.
