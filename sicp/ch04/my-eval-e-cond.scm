; handle cond-exp

(define (install-eval-cond)
  ; (cond <clause #1>
  ;       <clause #2>
  ;       ...
  ;       [(else ...)])

  ; a list of clauses
  (define cond-clauses cdr)

  ; each clause:
  ; (<exp1> <seq-of-exps>)
  ; <exp1>: predicate
  ; <seq-of-exps>: actions
  (define cond-predicate car)
  (define cond-actions cdr)

  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

  (define (cond->if exp)
    (define (expand-clauses clauses)
      (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (if (cond-else-clause? first)
            (if (null? rest)
              ; else part ... convert the seq to exp
              (sequence->exp (cond-actions first))
              (error "ELSE clause isn't last: COND->IF"
                     clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
    (expand-clauses (cond-clauses exp)))


  (define (eval-cond exp env)
    (my-eval (cond->if exp) env))

  (define (test)
    (define env
      (init-env))

    (define cond-text-exp-1
      `(cond ((= a 0) 2)
             ((= a 1) 1)
             ((= a 2) 0)))

    (define cond-text-exp-2
      `(cond ((= a 0) 10)
             (else 10 15 20)))

    (do-test
      eval-cond
      (list
        (mat cond-text-exp-1
             (extend-environment
               '(a) '(0) env) 2)
        (mat cond-text-exp-1
             (extend-environment
               '(a) '(1) env) 1)
        (mat cond-text-exp-1
             (extend-environment
               '(a) '(2) env) 0)
        (mat cond-text-exp-2
             (extend-environment
               '(a) '(0) env) 10)
        (mat cond-text-exp-2
             (extend-environment
               '(a) '(1) env) 20)
        ))
    'ok)

  (define handler
    (make-handler
      'cond
      eval-cond
      test))

  (handler-register! handler)

  'ok)

