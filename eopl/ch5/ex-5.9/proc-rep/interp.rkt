(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")
  (require "store.rkt")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env)   
            (lambda (val) 
              (begin
                (eopl:printf
                  "End of computation.~%")
                val)))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (deref (apply-env env var))))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (lambda (val)
              (apply-cont cont
                (bool-val
                  (zero? (expval->num val)))))))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (lambda (val)
              (value-of/k body
                (extend-env var (newref val) env) cont))))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (lambda (val)
              (value-of/k
                (if (expval->bool val) exp2 exp3)
                env
                cont))))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (lambda (val1)
              (value-of/k exp2 env
                (lambda (val2)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (apply-cont cont
                      (num-val (- num1 num2)))))))))
        (call-exp (rator rand) 
          (value-of/k rator env
            (lambda (rator-val)
              (value-of/k rand env
                (lambda (rand-val)
                  (let ((proc (expval->proc rator-val)))
                    (apply-procedure/k proc rand-val cont)))))))
        (begin-exp (exp1 exps)
          (define (begin-cont-maker exps cont)
            (lambda (val)
              (if (null? exps)
                (apply-cont cont val)
                (value-of/k (car exps) env
                  (begin-cont-maker (cdr exps) cont)))))
          (value-of/k exp1 env
            (lambda (val1)
              (if (null? exps)
                (apply-cont cont val1)
                (value-of/k (car exps) env
                  (begin-cont-maker (cdr exps) cont))))))
        (assign-exp (var exp1)
          (value-of/k exp1 env
            (lambda (val)
              (begin
                (setref!
                  (apply-env env var)
                  val)
                (apply-cont cont 'invalid)))))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cont val)))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 val cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var (newref val) saved-env)
            cont)))))
  
  )
  


  
