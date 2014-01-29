(define (install-eval-set!)

  (define assignment-variable cadr)
  (define assignment-value   caddr)

  (define (eval-set! exp env)
    (set-variable-value!
      (assignment-variable exp)
      (my-eval (assignment-value exp) env)
      env)
    'ok)

  ; ONLY RUN THIS TEST ONCE.
  ;   I don't know what is the fucking underlying implementation
  ;   but it does keep the previous environment
  ;   so side effect remains, the test will fail if you call this twice.
  ; TODO: workaround?
  ;   I don't think it worth a try to solve this,
  ;   so I might just leave it here forever.
  (define (test)
    ; test 3-layer environments

    ; env -> env1 -> env2
    ;            \-> env3
    ; workaround: use (list ...) rather than '(...)
    ;   not sure why, but it works anyway
    (define env
      (extend-environment
        (list 'a 'b 'c)
        (list 1 2 3)
        the-empty-environment))
    (define env1
      (extend-environment
        (list 'c 'd 'e)
        (list #\c #\d #\e)
        env))
    (define env2
      (extend-environment
        (list 'd 'e 'f)
        (list "d" "e" "f")
        env1))
    (define env3
      (extend-environment
        (list 'a 'b 'c)
        (list "a3" "b3" "c3")
        env1))

    (eval-set! '(set! a "ax") env3)

    ; should only have effects on `env3`
    (do-test
      lookup-variable-value
      (list
        (mat 'a env  1)
        (mat 'a env1 1)
        (mat 'a env2 1)
        (mat 'a env3 "ax"))
      equal?)
    
    ; anything other than `env3` should be changed
    (eval-set! '(set! a "ay") env1)

    (do-test
      lookup-variable-value
      (list
        (mat 'a env  "ay")
        (mat 'a env1 "ay")
        (mat 'a env2 "ay")
        (mat 'a env3 "ax"))
      equal?)
    'ok)
  
  (define handler
    (make-handler
      'set!
      eval-set!
      test))

  (handler-register! handler)
  'ok)
