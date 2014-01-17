(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")

  (require (only-in racket/base
                    foldl))

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        (cons-exp (exp1 exp2)
          (let ((expval1 (value-of exp1 env))
                (expval2 (value-of exp2 env)))
            (list-val
              (cons expval1 (expval->list expval2)))))

        (car-exp (exp1)
          (car (expval->list (value-of exp1 env))))

        (cdr-exp (exp1)
          (list-val (cdr (expval->list (value-of exp1 env)))))

        (null?-exp (exp1)
          (bool-val (null? (expval->list (value-of exp1 env)))))

        (empty-list-exp ()
          (list-val '()))

        (unpack-exp (vars explist body)
          (let ((expvals (expval->list (value-of explist env))))
            (if (not (= (length vars) (length expvals)))
              (eopl:error 'value-of
                "number of variables(~A) ~
                 does not match ~
                 with the number ~
                 of list elements(~A)"
                 (length vars)
                 (length expvals))
              ; now bind variables
              (let ((new-env
                      (foldl
                        extend-env
                        env
                        vars
                        expvals)))
                (value-of body new-env)))))
                          
          
          

        )))


  )

