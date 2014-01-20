(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the procedural
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")

  (require (only-in racket foldl remove-duplicates remove flatten))

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
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
        
        (proc-exp (var body)
          (proc-val (procedure
                      var
                      body
                      (free-variables exp))))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg env)))

        )))

  ; free-variables: Exp -> [Identifier]
  ; get a list of all free variables from `exp`
  (define (free-variables exp)
    (cases expression exp
           (const-exp (num)
             '())
           (var-exp (var)
             (list var))
           (diff-exp (exp1 exp2)
             (remove-duplicates
               (flatten
                 (list
                   (free-variables exp1)
                   (free-variables exp2)))))
           (zero?-exp (exp1)
                      (free-variables exp1))
           (if-exp (exp1 exp2 exp3)
             (remove-duplicates
               (flatten
                 (list
                   (free-variables exp1)
                   (free-variables exp2)
                   (free-variables exp3)))))
           (let-exp (var exp1 body)
             (remove-duplicates
               (flatten
                 (list
                   (free-variables exp1)
                   (remove var (free-variables body))))))
           (proc-exp (var body)
             (remove var (free-variables body)))
           (call-exp (rator rand)
             (remove-duplicates
               (flatten
                 (list
                   (free-variables rator)
                   (free-variables rand)))))))

  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (var body free-vars)
      (lambda (val env)
        (define proc-env
          (foldl
            extend-env
            (init-env)
            free-vars
            (map (lambda (v)
                   (apply-env env v))
                 free-vars)))
        (value-of body (extend-env var val proc-env)))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc val env)
      (proc val env)))

  )
