; handle if-exp

(define (install-eval-if)

  ; (if <if-predicate>
  ;   <if-consequent>
  ;   [<if-alternative>])
  (define if-predicate cadr)
  (define if-consequent caddr)
  (define (if-alternative exp)
    ; allowing the else part not filled.
    (if (non-empty? (cdddr exp))
      (cadddr exp)
      'false))

  (define (eval-if exp env)
    ; use `true?` here allows the language to be implemented
    ;   having a different representaion of truth value
    (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

  (define (test)
    (define env
      (extend-environment
        '(true false)
        '(#t #f)
        the-empty-environment))

    (define testcases
      (list
        (mat '(if 1 10) env 10)
        (mat '(if 1 10 20) env 10)
        (mat '(if #t 10 20) env 10)
        (mat '(if 'a 10 20) env 10)
        (mat '(if '#f 10 20) env 20)
        (mat '(if #f 10 20) env 20)
        (mat '(if true 10 20) env 10)
        (mat '(if 'false 10 20) env 10)
        (mat '(if false 10 20) env 20)
        ))
    (do-test eval-if testcases)
    'todo)

  (define handler
    (make-handler
      'if
      eval-if
      test))

  (handler-register! handler)
  'done)
