#lang racket

(require "basic-math.rkt"
         "package-logic.rkt"

         "rectangular-package.rkt"
         "polar-package.rkt"
         "rational-package.rkt"
         "complex-package.rkt")

;; ==========================
;; CODE OF PREVIOUS EXERCISES
;; ==========================
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; ==========================
;; HELP MAKE IT RUNNABLE CODE
;; ==========================
(define *coercion-table* (make-hasheq))

(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) #f))

(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

(install-coercion-package)

;; =====================
;; TOPIC START: COERCION
;; =====================

(define (apply-generic operation . args)
  ;; getting all type tags
  (let ([type-tags (map type-tag args)])
    ;; getting the specific operation from the table of operations
    ;; for the type tags we got
    (let ([procedure (get operation type-tags)])
      (cond [procedure  ;; if there is such a procedure
             (apply procedure (map contents args))]  ;; we apply it to the contents of all arguments (which are data of some types) :) for which we found a procedure
            [(= (length args) 2)  ;; if we have 2 arguments
             ;; if we have 2 arguments, we get the types into the constants type1 and type2
             ;; and get the arguments separated in arguments1 and arguments2
             (let ([type1 (car type-tags)]
                   [type2 (cadr type-tags)]
                   [arguments1 (car args)]
                   [arguments2 (cadr args)])

               (if (equal? type1 type2)                                            ;; 2.81.c: added this condition
                   (error "No method for these types" (list operation type-tags))  ;; 2.81.c: added this error
                                                                                   ;; 2.81.c: indented stuff below
                   ;; with these constants
                   ;; we find the coercion procedures for the types
                   (let ([t1->t2 (get-coercion type1 type2)]
                         [t2->t1 (get-coercion type2 type1)])
                     (cond
                      ;; if there is a coercion procedure for coercion from type1 to type2
                      [t1->t2
                       ;; we apply the operation to the coerced arguments1 and not coerced arguments2
                       (apply-generic operation (t1->t2 arguments1) arguments2)]
                      ;; if there is a coercion procedure for coercion from type2 to type1
                      [t2->t1
                       ;; we apply the operation to the not coerced arguments1 and coerced arguments2
                       (apply-generic operation arguments1 (t2->t1 arguments2))]

                      [else (error "No method for these types" (list operation type-tags))]))))]

            [else  ;; if the arguments are more or less than 2 arguments we throw an error
             (error "No method for these types" (list operation type-tags))]))))

;; EXERCISE 2.81

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (scheme-number->scheme-number n) n)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(define (complex->complex z) z)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
