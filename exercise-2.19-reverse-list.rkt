#lang racket
(require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...

(define nil '())

(define one-through-four (list 1 2 3 4 5))

(define (length mylist)
  (define (iter a count)
    (if
      (null? a)
      count
      (iter (cdr a) (+ count 1))))
  (iter mylist 0))

(define (last mylist)
  (if
    (null? (cdr mylist))
    mylist
    (last-pair (cdr mylist))))

(define (append list1 list2)
;  (display "appending the lists: ") (display list1) (display " and ") (display list2) (newline)
  (if
    (null? list1)
    list2
    (cons
      (car list1)
      (append (cdr list1) list2))))

(define (drop-last mylist)
  (define (iter sublist reslist)
    (if
      (null? (cdr sublist))
      reslist
      (iter
        (cdr sublist)
        (append
          reslist
          (list (car sublist))))))
  (if
    (< (length mylist) 2)
    nil
    (iter mylist nil)))

(define mylist (list 1 2 3 4 5 6 7 8 9 10))
(display "mylist is: ") (display mylist) (newline)
(display "mylist dropped last is: ") (display (drop-last mylist)) (newline)

;; my own solution, waaaaay too complicated! (but works)
;(define (reverse mylist)
;  (define (iter sublist reslist)
;    (if
;      (null? sublist)
;      reslist
;      (iter
;        (drop-last sublist)
;        (append reslist (last-pair sublist)))))
;  (iter mylist nil))

;; found this one online
(define (reverse items)
  (define (iter subitems result)
    (if
      (null? subitems)
      result
      (iter (cdr subitems) (cons (car subitems) result))))
  (iter items nil))

(display "mylist reversed is: ")
(time (reverse mylist)) (newline)
