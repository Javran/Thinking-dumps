; tests are copied from ./my-eval-env.scm
(define (test-environment
          lookup-variable-value
          extend-environment
          define-variable!
          set-variable-value!)
  (define env (extend-environment
                '(a b c)
                '(1 2 3)
                the-empty-environment))

  ; rebind some variables
  (define-variable! 'c 'cval env)
  (define-variable! 'b 'bval env)
  
  ; define new variable
  (define-variable! 'd 'd env)

  ; test lookup-variable-value
  (do-test
    lookup-variable-value
    (list
      (mat 'a env 1)
      (mat 'c env 'cval)
      (mat 'd env 'd)
      (mat 'b env 'bval)))

  ; new env
  (define env1 (extend-environment
                 '(c d e f)
                 '(10 20 30 40)
                 env))
  ; another new
  (define env2 (extend-environment
                 '(b)
                 '(bb22)
                 env))
  
  ; test lookup-variable-value in nested env structure
  (do-test
    lookup-variable-value
    (list
      ; from enclosing env
      (mat 'a env1 1)
      (mat 'b env1 'bval)
      ; shadowed
      (mat 'c env1 10)
      (mat 'd env1 20)
      ; newly created
      (mat 'e env1 30)
      (mat 'f env1 40)))

  ; define new variable
  (define-variable! 'g '50  env1)
  (define-variable! 'a 'aaa env1)

  ; modify existing
  (define-variable! 'c 'ccc env1)

  ; env shouldn't change, redo tests again on `env`
  (do-test
    lookup-variable-value
    (list
      (mat 'a env 1)
      (mat 'c env 'cval)
      (mat 'd env 'd)
      (mat 'b env 'bval)))

  ; new env1 test
  (do-test
    lookup-variable-value
    (list
      (mat 'a env1 'aaa)
      (mat 'b env1 'bval)
      (mat 'c env1 'ccc)
      (mat 'd env1 20)
      (mat 'e env1 30)
      (mat 'f env1 40)
      (mat 'g env1 50)))

  ; test set!
  (set-variable-value! 'b 'bbb env1)

  ; b's binding should all be changed
  (do-test
    lookup-variable-value
    (list
      (mat 'b env  'bbb)
      (mat 'b env1 'bbb)
      (mat 'b env2 'bb22)))

  ; set! on `env2` should not change bindings
  ;   in `env` and `env1` however.
  (set-variable-value! 'b 'new-b env2)

  (do-test
    lookup-variable-value
    (list
      (mat 'b env  'bbb)
      (mat 'b env1 'bbb)
      (mat 'b env2 'new-b)))
  
  'ok)
