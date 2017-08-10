#|
"Coercion" is the process of viewing data of a specific type as data of another
type and acting accordingly. For example an integer number might be viewed as a
rational number and added to another rational number, by a procedure, which
only can work with rational numbers.
|#
(define (apply-generic operation . args)
  ;; getting all type tags
  (let ([type-tags (map type-tag args)])
    ;; getting the specific operation from the table of operations
    ;; for the type tags we got
    (let ([procedure (get operation type-tags)])
      (cond [procedure  ;; if there is such a procedure
             (apply procedure (map contents args))]  ;; we simply apply it to the contents of all arguments (which are data of some types) :) for which we found a procedure
            [(= (length args) 2)  ;; if we have 2 arguments
             ;; if we have 2 arguments, we get the types into the constants type1 and type2
             ;; and get the arguments separated in arguments1 and arguments2
             (let ([type1 (car type-tags)]
                   [type2 (cadr type-tags)]
                   [arguments1 (car args)]
                   [arguments2 (cadr args)])
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

                  [else (error "No method for these types" (list operation type-tags))])))]

            [else  ;; if the arguments are more or less than 2 arguments we throw an error
             (error "No method for these types" (list operation type-tags))]))))

#|
By the logic of this code, type1 is always coerced if there is a coercion procedure from type1 to type2, even if there is one from type2 to type1 as well.
Louis Reasoner says, that the apply-generic procedure coerces types, even if type1 and type2 are the same types.
If there are no coercion procedures available for the same types case, the apply-generic procedure will run into an error case.
However, if both types are the same, the apply-generic procedure would not enter the branch for finding a coercion procedure for the types, if there was a suitable operation found.
So by coercion to the same type, we would gain nothing, because it would not change the types and there would still not be any suitable operation for the two equal types.

Answer to 2.81.a:
If complex can be coerced scheme-number, we will not be able to coerce, because we will be coercing to the same type.
Then apply-generic will be called with the same types again, which will lead to an endless loop.
If complex cannot be coerced to scheme-number, we will run into an error (the inner one).

Answer to 2.81.b:
Nothing had to be done about same types.

Answer to 2.81.c:
(see code below)
|#

;; Exercise 2.81
;; a. Louis Reasoner adds:
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; also we added
;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y)
       (tag (expt x y))))
; using primitive expt

;; What happens if we then call the following for complex numbers?
(define (exp x y)
  (apply-generic 'exp x y))

(define (apply-generic-modified operation . args)
  ;; getting all type tags
  (let ([type-tags (map type-tag args)])
    ;; getting the specific operation from the table of operations
    ;; for the type tags we got
    (let ([procedure (get operation type-tags)])
      (cond [procedure  ;; if there is such a procedure
             (apply procedure (map contents args))]  ;; we simply apply it to the contents of all arguments (which are data of some types) :) for which we found a procedure
            [(= (length args) 2)  ;; if we have 2 arguments
             ;; if we have 2 arguments, we get the types into the constants type1 and type2
             ;; and get the arguments separated in arguments1 and arguments2
             (let ([type1 (car type-tags)]
                   [type2 (cadr type-tags)]
                   [arguments1 (car args)]
                   [arguments2 (cadr args)])

               (if (equal type1 type2)                                             ;; 2.81.c: added this condition
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
                       (apply-generic-modified operation (t1->t2 arguments1) arguments2)]
                      ;; if there is a coercion procedure for coercion from type2 to type1
                      [t2->t1
                       ;; we apply the operation to the not coerced arguments1 and coerced arguments2
                       (apply-generic-modified operation arguments1 (t2->t1 arguments2))]

                      [else (error "No method for these types" (list operation type-tags))]))))]

            [else  ;; if the arguments are more or less than 2 arguments we throw an error
             (error "No method for these types" (list operation type-tags))]))))
