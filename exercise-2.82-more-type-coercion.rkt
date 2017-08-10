;; Louis Reasoner adds:
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex
              complex->complex)

(put 'exp '(scheme-number scheme-number)
     (lambda (x y)
       (tag (expt x y))))

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

#|
Exercise 2.82:

Hint:
Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.

2.82.a:
Show how to generalize apply-generic to handle coercion in the general case of multiple arguments.
One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on.
|#
(define (coerce-types list-of-types type-to-coerce-to)
  (define (coerce-type source-type target-type)
    ;; get the coercion
    ())


#|
2.82.b:
Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general.
|#
