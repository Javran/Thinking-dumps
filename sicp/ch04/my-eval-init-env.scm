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
        )
      equal?)))
