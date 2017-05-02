;; "Coercion" is the process of viewing data of a specific type as data of another type and acting accordingly.
;; For example an integer number might be viewed as a rational number and added to another rational number, by a procedure, which only can work with rational numbers.
(define (apply-generic operation . args)
  ;; getting all type tags
  (let ((type-tags (map type-tag args)))
    ;; getting the specific operation from the table of operations
    ;; for the type tags we got
    (let ((procedure (get operation type-tags)))
      (if
        ;; if there is such a procedure
        procedure
        ;; we simply apply it to the contents of all arguments (which are data of some types) :)
        (apply procedure (map contents args))
        (if
          ;; otherwise, we check if we have 2 arguments
          (= (length args) 2)
          ;; if we have 2 arguments, we get the types into the constants type1 and type2
          ;; and get the arguments separated in a1 and a2
          (let ((type1 (car type-tags))
                 (type2 (cadr type-tags))
                 (a1 (car args))
                 (a2 (cadr args)))
            ;; with these constants
            ;; we find the coercion procedures for the types
            (let ((t1->t2 (get-coercion type1 type2))
                   (t2->t1 (get-coercion type2 type1)))
              (cond
                ;; if there is a coercion procedure for coercion from type1 to type2
                (t1->t2
                  ;; we apply the operation to the coerced a1 and not coerced a2
                  (apply-generic operation (t1->t2 a1) a2))
                ;; if there is a coercion procedure for coercion from type2 to type1
                (t2->t1
                  ;; we apply the operation to the not coerced a1 and coerced a2
                  (apply-generic operation a1 (t2->t1 a2)))
                ;; otherwise
                (else
                  ;; we print an error
                  (error "No method for these types"
                    (list operation type-tags))))))
          ;; if the arguments are more or less than 2 arguments we throw an error
          (error "No method for these types"
            (list operation type-tags)))))))

;; Exercise 2.81
;; a. Louis Reasoner adds:
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion
  'scheme-number
  'scheme-number
  scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; also we added
;; following added to Scheme-number package
(put
  'exp
  '(scheme-number scheme-number)
  (lambda (x y)
    (tag (expt x y))))
; using primitive expt

;; What happens if we then call the following for complex numbers?
(define (exp x y) (apply-generic 'exp x y))



;; TODO
;; b. Is Louis correct that something had to be done about coercion with arguments of the same type, or does apply-generic work correctly as is?
;; c. Modify apply-generic so that it doesn’t try coercion if the two arguments have the same type.
