#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)


(define (square x) (* x x))

;; make-from-real-imag returns a procedure, which is internally names "dispatch",
;; because it dispatches an operation name given to it.
;; Depending on what operation it receives, it returns a different value.
;; This is what it means to answer a "message".
;; This is why it is called message passing style.
;; If it receives a procedure, which it does not know,
;; an error is raised.
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)
;; One disadvantage of this style is, that it allows only one parameter per call.
;; However, this might motivate the programmer to choose encapsulations of data,
;; so that they can pass them in one message.

;; Now follows the generic apply procedure.
;; It receives an operation op and an object arg,
;; which will be called with the operation as a message.
;; The object arg can then dispatch on the provided operation's name.
(define (apply-generic op arg)
  (arg op))


;; EXERCISE 2.75

;; a. Implement the constructor make-from-mag-ang in message-passing style.
;;    This procedure should be analogous to the make-from-real-imag procedure given above.

(define (make-from-mag-ang a-radius an-angle)
  (define (dispatch op)
    (cond
      [(eq? op 'real-part)
        (* a-radius (cos an-angle))]
      [(eq? op 'imag-part)
        (* a-radius (sin an-angle))]
      [(eq? op 'magnitude) a-radius]
      [(eq? op 'angle) an-angle]
      [else (error "Unknown op: MAKE-FROM-MAG-ANG" op)]))
  dispatch)
