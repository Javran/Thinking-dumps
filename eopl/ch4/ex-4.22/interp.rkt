(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")
  (require "store.rkt")
  (require "printer.rkt")
  (require (only-in racket foldl))
  
  (provide value-of-program value-of instrument-let instrument-newref)

  (provide result-of-program result-of)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (initialize-printer!)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  (define (result-of-program pgm)
    (initialize-store!)
    (initialize-printer!)
    (cases program pgm
      (a-program (stmt)
        (result-of stmt (init-env)))))

  ; should return the environment modified
  (define (result-of stmt env)
    (cases statement stmt
      (assign-stmt (var exp1)
        (begin
          (define val1 (value-of exp1 env))
          (define loc1 (apply-env env var))
          (setref! loc1 val1)
          env))
      (print-stmt (exp1)
        (begin
          (define val1
            (expval->printable (value-of exp1 env)))
          (send-to-printer
            (cases expval val1
              (num-val (n) n)
              (bool-val (b) b)
              (proc-val (p) p)
              (else "<unknown>")))
          env))
      (chain-stmt (stmts)
        (if (null? stmts)
          ; empty list, done
          env
          (result-of (chain-stmt (cdr stmts))
                     (result-of (car stmts) env))))
      (if-stmt (exp1 stmt-conseq stmt-alter)
        (begin
          (define val1 (expval->bool (value-of exp1 env)))
          (if val1
            (result-of stmt-conseq env)
            (result-of stmt-alter env))))
      (while-stmt (exp1 stmt1)
        (begin
          (define val1 (expval->bool (value-of exp1 env)))
          (if val1
            ; call for `stmt1`'s side effect, and use the modified env
            (result-of
              (while-stmt exp1 stmt1)
              (result-of stmt1 env))
            env)))
      (var-stmt (vars inner-stmt)
        (begin
          ; the initial value is not specified
          ;   so anything will be acceptable
          (define locs (map (lambda (l)
                              (newref (bool-val #f)))
                            vars))
          (define new-env
            (foldl extend-env env vars locs))
          (result-of inner-stmt new-env)
          ; var only has effects on inner-stmt
          ;   we should restore the previous env
          env))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        (sum-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))

        (prod-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))

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

        (not-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (bool-val
              ; can use `not` here
              ;   but I want to make sure
              ;   the return value is either #t or #f
              (if (expval->bool val1)
                #f #t))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map
                        (lambda (rand) (value-of rand env))
                        rands)))
            (apply-procedure proc args)))

        (letrec-exp (p-names b-varss p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-varss p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((refs (map
                        (lambda (arg)
                          (newref arg))
                        args)))
            (let ((new-env (foldl extend-env saved-env vars refs)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    vars)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))  

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
