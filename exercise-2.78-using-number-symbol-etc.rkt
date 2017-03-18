#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; (define (attach-tag type-tag contents)
;;   (cond
;;     [(number? (car contents)) contents]
;;     [(symbol? (car contents)) (cons type-tag contents)]
;;     [else (cons type-tag contents)]))

;; (define (type-tag datum)
;;   (if (pair? datum)
;;     (cond
;;       [(number? (car datum)) 'scheme-number]
;;       [(symbol? (car datum)) (car datum)]
;;       [else (car datum)])
;;     (error "Bad tagged datum: TYPE-TAG" datum)))

;; (define (contents datum)
;;   (if (pair? datum)
;;     (cond
;;       [(number? (car datum)) datum]
;;       [(symbol? (car datum)) (cdr datum)]
;;       [else (cdr datum)])
;;     (error "Bad tagged datum: CONTENTS" datum)))


(define (attach-tag type-tag contents)
  (cond
    ;; not assuming pairs
    [(number? contents) contents]
    [(symbol? (car contents)) (cons type-tag contents)]
    [(pair? contents) (cons type-tag contents)]
    [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond
    [(number? datum) 'scheme-number]
    [(symbol? (car datum)) (car datum)]
    [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(number? datum) datum]
    [(symbol? datum) nil]
    [(pair? datum) (cdr datum)]
    [else (error "Bad tagged datum: CONTENTS" datum)]))
