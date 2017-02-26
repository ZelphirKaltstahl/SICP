;; Data-directed Programming is a technique, which makes use of a table, in which programs look up procedures they can use to work with a specific type of data.
;; Here is an example of this.

(define (install-rectangular-package)
  ;; internal procedures,
  ;; (the one Ben defines for working with his representation)
  ;; They simply assume rectangular form of complex numbers and will be added to the table of operations only for rectangular form.
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt
      (+
        (square (real-part z))
        (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  ;; (this code is required to register Ben's procedures work with the system)
  ;; This includes registering Ben's procedures and adding a procedure for tagging data with a specific tag for rectangular form of complex numbers.
  (define (tag x)
    (attach-tag 'rectangular x))
  ;; Ben needs to know about the table mechanism for looking up suitable operations for types.
  ;; Ben registers his procedures.
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  ;; here the naive make-from-real-imag gets the additional functionality of adding a tag for Ben's representation
  (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
  ;; here the naive make-from-mag-ang gets the additional functionality of adding a tag for Ben's representation
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; Note that Ben did not have to rename his procedures in any way, because they are defined in the scope of install-rectangular-package.
  'done)

;; In contrast now look at Alyssa's install-rectangular-package procedure.
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
      (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y)
      (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
    (lambda (r a)
      (tag (make-from-mag-ang r a))))
  'done)

;; to lookup which operation from the operations table should be used, a generic procedure is needed:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

;; Then when looking up the suitable operation is possible, we can define generic selectors, which work with complex numbers and look up the suitable operations in the table of operations as follows:

;; These will not change when a new representation is added.
;; New ones might be added, for new representations.
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Also one can formulate constructors generic constructors, which utilize constructors defined by external programs.
;; New ones might be added for new representations, but these do not need to change.
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

