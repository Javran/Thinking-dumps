; handle and-exp
; from ./exercise 4_4_a.scm

(define (install-eval-and)
  (define (eval-and exp env)
    ; evaluate expressions, short circuit if necessary
    (define (eval-and-aux exps env)
      (cond ((null? exps) #t)
            ((null? (cdr exps))
             ; we are dealing with the last element in the list
             ; simply evaluating it will do
             (my-eval (car exps) env))
            (else
              (if (true? (my-eval (car exps) env))
                ; need further examination
                (eval-and-aux (cdr exps) env)
                ; else short circuit
                #f))))
    (eval-and-aux (cdr exp) env))

  (define (test)
    (let ((env (init-env)))
      (do-test
        eval-and 
        (list
          (mat '(and) env #t)
          (mat '(and (= 1 1) (= 2 2)) env #t)
          (mat '(and (= 1 1) #f (error 'wont-reach)) env #f)
          (mat '(and 1 2 3 4) env 4)
          (mat '(and 1) env 1)
          ))
      'ok))

  (define handler
    (make-handler
      'and
      eval-and
      test))

  (handler-register! handler)
  'ok)
