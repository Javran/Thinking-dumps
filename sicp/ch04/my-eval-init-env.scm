; lookup a primitive in implementing language
;   lift it into the implemented language
;   return a pair,
;   whose `car` is the symbol,
;     and `cdr` is the value (the primitive lifted)
(define (lift-primitive-pair sym)
  (cons sym
        (make-proc-primitive
          (environment-lookup
            user-initial-environment sym))))

; initialize an environment that contains basic stuffs
(define (init-env)
  (let ((proc-list
          (list
            (lift-primitive-pair '+)
            (lift-primitive-pair '-)
            (lift-primitive-pair '*)
            (lift-primitive-pair '/)
            (lift-primitive-pair '=)
            (lift-primitive-pair '>)
            (lift-primitive-pair '>=)
            (lift-primitive-pair '<)
            (lift-primitive-pair '<=)
            (lift-primitive-pair 'zero?)
            (lift-primitive-pair 'eq?)
            (lift-primitive-pair 'eqv?)
            (lift-primitive-pair 'car)
            (lift-primitive-pair 'cdr)
            (lift-primitive-pair 'cons)
            (lift-primitive-pair 'null?)
            (lift-primitive-pair 'list)
            (lift-primitive-pair 'even?)
            (lift-primitive-pair 'odd?)
            (lift-primitive-pair 'not)
            (lift-primitive-pair 'remainder)
            (lift-primitive-pair 'quotient)
            (lift-primitive-pair 'sqrt)
            (lift-primitive-pair 'integer?)
            (lift-primitive-pair 'member)
            (lift-primitive-pair 'delete)
            (lift-primitive-pair 'abs)
            )))
    (extend-environment
      '(true false)
      '(#t   #f)
      (extend-environment
        (map car proc-list)
        (map cdr proc-list)
        the-empty-environment))))

(define (test-init-env)
  (let ((env (init-env)))
    (do-test
      my-eval
      (list
        ; just pick some randomly to perform a test
        ;   I think it's not necessary to test them all
        (mat '(+ 1 2 3) env 6)
        (mat '(- 7 1 2 3) env 1)
        (mat '(/ 1024 256) env 4)
        (mat '(= 1 1) env #t)
        (mat '(= 1 0) env #f)
        (mat '(zero? 0) env #t)
        (mat '(eq? 'a 'a) env #t)
        (mat '(eq? 'a 'b) env #f)
        ; compound
        (mat '(= (* 1 2 3 4) (* 2 (+ 10 2))) env #t)
        ; test list-related primitives
        (mat `(cons 1 '(2 3)) env '(1 2 3))
        (mat `(cons 'a 'b) env '(a . b))
        (mat `(car '(1 2)) env 1)
        (mat `(cdr '(1 2)) env '(2))
        (mat `(null? '()) env #t)
        (mat `(null? '(1)) env #f)
        (mat `(list 'a 'b) env '(a b))
        )
      equal?)))
