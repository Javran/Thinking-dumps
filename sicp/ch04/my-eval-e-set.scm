(define (install-eval-set!)

  (define assignment-variable cadr)
  (define assignment-value   caddr)

  (define (eval-set! exp env)
    (set-variable-value!
      (assignment-variable exp)
      (my-eval (assignment-value exp) env)
      env)
    'ok)

  (define (test)
    ; test 3-layer environments

    ; env -> env1 -> env2
    ;            \-> env3
    (define env
      (extend-environment
        '(a b c)
        '(1 2 3)
        the-empty-environment))
    (define env1
      (extend-environment
        '(c d e)
        '(#\c #\d #\e)
        env))
    (define env2
      (extend-environment
        '(d e f)
        '("d" "e" "f")
        env1))
    (define env3
      (extend-environment
        '(a b c)
        '("a3" "b3" "c3")
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
    'done)
  
  (define handler
    (make-handler
      'set!
      eval-set!
      test))

  (handler-register! handler)
  'done)
