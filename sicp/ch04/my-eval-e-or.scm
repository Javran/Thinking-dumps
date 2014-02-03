; handle or-exp
; from ./exercise 4_4_a.scm

(define (install-eval-or)
  (define (eval-or exp env)
    ; evaluate expressions
    (define (eval-or-aux exps env)
      (cond ((null? exps) #f)
            ((null? (cdr exps))
             (my-eval (car exps) env))
            (else
              (let ((result (my-eval (car exps) env)))
                (if (true? result)
                  result
                  (eval-or-aux (cdr exps) env))))))
    (eval-or-aux (cdr exp) env))

  (define (test)
    (let ((env (init-env)))
      (do-test
        eval-or
        (list
          (mat '(or) env #f)
          (mat '(or #t (error 'wont-reach)) env #t)
          (mat '(or (< 1 1) (> 2 2)) env #f)
          (mat '(or (quote symbol)) env 'symbol)
          (mat '(or #f #f 1) env 1)
          ))
      'ok))

  (define handler
    (make-handler
      'or
      eval-or
      test))

  (handler-register! handler)
  'ok)
