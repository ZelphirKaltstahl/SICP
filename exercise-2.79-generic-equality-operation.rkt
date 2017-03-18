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
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

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
  (put 'equ? '(scheme-number scheme-number)
    (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
    (lambda (x) (= x 0)))
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

  (define (equ? ratio1 ratio2)
    (let
      ([factor-numer (/ (numer ratio2) (numer ratio1))]
       [factor-denom (/ (denom ratio2) (denom ratio1))])
      (= factor-numer factor-denom)))

  (define (=zero? x)
    (= (numer x) 0))

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
  (put 'equ? '(rational rational)
    equ?)
  (put '=zero? '(rational)
    =zero?)
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


;; COMPLEX NUMBERS
;; In contrast to rational number and ordinary numbers complex numbers have two separate representations.
;; This means that this package will be using generic operations, which are generic regarding the fact that there are two representations for complex numbers, but specific to complex numbers.
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  ;; Quote from the book:
  ;; "Notice how the underlying procedures, originally defined in the rectangular and polar packages, are exported to the complex package, and exported from there to the outside world."
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; Internal Procedures
  ;; Since these are internally defined and not visible from the outside,
  ;; we could also give them generic names like add, sub, mul or div.
  ;; They would not override other procedures outside this package.
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))

  (define (equ? z1 z2)
    (and
      (= (real-part z2) (real-part z1))
      (= (imag-part z2) (imag-part z1))))

  (define (=zero? z)
    (= (real-part z) (imag-part z) 0))

  ;; interface to rest of the system
  (define (tag z)
    (attach-tag 'complex z))

  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
    equ?)
  (put '=zero? '(complex)
    =zero?)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;; Exercise 2.77
;; Louis Reasoner has the following object z:
;; (list 'complex (list 'rectangular (cons 3 4)))
;;
;; He tries to calculate:
;; (magnitude z)
;;
;; He gets an error saying that there is no method for operation magnitude for data tagged 'complex.
;; Alyssa tells him to add the following to make it work:
;; (in a scope, where real-part, imag-part, magnitude and angle are defined)
;; (in the package for complex)
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; A) Describe in detail why this works.

;; Well, it's not actually true, since there is no magnitude in the package for 'complex.
;; One would need to add one there first.
;; The reason why the one inside 'polar package or 'rectangular package do not work is, that they are not aware ot the tagging system. They act as if there was only one representation and are not generic.
;; The added magnitude procedure would have to perform a lookup in the table of operations itself.
;; It would have to be something like this: (define (magnitude z) (get (cadr z) (cdr z)))

;; Explanation:
;; The procedure magnitude is defined for 'polar and 'rectangular.
;; 1. When the lookup in the table of operations happens, the program looks up the operation magnitude for complex, which is not defined, just as Alyssa told Louis.
;; 2. However, if we add the operation to the table of operations, the lookup will be successful.
;; 3. We will get back the procedure magnitude.
;; 4. This procedure is then applied on the remaining data, which is tagged 'rectangular.
;; 5. For 'rectangular magnitude was originally already defined.

;; B) As an example, trace through all the procedures called in evaluating the expression (magnitude z) where z is the object shown in Figure 2.24. In particular, how many times is apply-generic invoked? What procedure is dispatched to in each case?


;; There is some mistake here. There is no procedure magnitude for the 'complex package, so the first thing failing would be:

 ;; (put 'magnitude '(complex) magnitude)

;; In the code of the book the package does not contain such a procedure magnitude. It seems to assume that Alyssa also writes that code, but in the exercise 2.77 does not say so. The book also does not state in which package the code should be added. The specific procedures magnitude inside the packages rectangular and polar do not work for complex, because they assume, that they are working with either representation and are not aware of the additional tag 'complex attached to the data. So they would extract the wrong pieces from the data and then fail. The implementation of magnitude inside the complex package would probably have to unwrap the tagged data by one tag, the complex tag, and perform a lookup in the table of operations itself:

 ;; (define (magnitude z) (get 'magnitude (cadr z) (cdr z)))
 ;; get operation for magnitude of the data wrapped inside the data.
 ;; (cadr z) would become 'rectangular
 ;; (cdr z) would become (list 'rectangular (cons 3 4))

;; So basically with the information given in the book the tracing (as demanded in the exercise) of the calls would fail (correct me if I am wrong about this please). However, under reasonable assumptions, like a defined magnitude in the package for complex, I think the answer of meteorgan is correct.
