#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; EXERCISE 2.73
;; GIVEN CODE

;; ARITHMETIC OPERATIONS
;; They can be defined to be implementation independent, since the selectors can deal with the details of implementation, by checking for the tags in tagged data.
;; (highest level of abstraction, LEVEL 3)
(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

;; The key to implementing this flexibility are the mathematical relationships between the parts of each representation. They allow us to define the operations in both ways, by implementing the conversion formulas in code.

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; PREDICATES
;; ==========
;; abstraction LEVEL 1
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; SELECTORS
;; =========
;; implementation specific, LEVEL 1
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
          (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
    (real-part-rectangular z)))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))



;; general ones
(define (type-tag datum)
  (if
    (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if
    (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

;; abstraction LEVEL 2
;; implementation flexible, checking tags with predicates
(define (real-part z)
  (cond
    [(rectangular? z)
      (real-part-rectangular (contents z))]
    [(polar? z)
      (real-part-polar (contents z))]
    [else (error "Unknown type: REAL-PART" z)]))

(define (imag-part z)
  (cond
    [(rectangular? z)
      (imag-part-rectangular (contents z))]
    [(polar? z)
      (imag-part-polar (contents z))]
    [else
      (error "Unknown type: IMAG-PART" z)]))

(define (magnitude z)
  (cond
    [(rectangular? z)
      (magnitude-rectangular (contents z))]
    [(polar? z)
      (magnitude-polar (contents z))]
    [else
      (error "Unknown type: MAGNITUDE" z)]))

(define (angle z)
  (cond
    [(rectangular? z)
      (angle-rectangular (contents z))]
    [(polar? z)
      (angle-polar (contents z))]
    [else
      (error "Unknown type: ANGLE" z)]))

;; LOWER LEVEL CONSTRUCTORS
;; representation specific, LEVEL 1
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
    (cons (* r (cos a)) (* r (sin a)))))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
    (cons (sqrt (+ (square x) (square y)))
      (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; HIGHER LEVEL CONSTRUCTORS
;; abstracting the representation created from specific given parameters
;; if we have real and imag part, it is natural to create the representation using those parts
;; LEVEL 3 (?)
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
;; if we have magnitude and angle part, it is natural to create the representation using those parts
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
