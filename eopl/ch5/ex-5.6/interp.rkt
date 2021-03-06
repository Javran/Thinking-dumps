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

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))
        (empty-list-exp ()
          (apply-cont cont (list-val '())))
        (cons-exp (exp1 exp2)
          (value-of/k exp1 env
            (cons1-cont exp2 env cont)))
        (null?-exp (exp1)
          (value-of/k exp1 env
            (null?-cont cont)))
        (car-exp (exp1)
          (value-of/k exp1 env
            (car-cont cont)))
        (cdr-exp (exp1)
          (value-of/k exp1 env
            (cdr-cont cont)))
        (list-exp (exps)
          (if (null? exps)
            ; emptylist
            (apply-cont cont (list-val '()))
            ; eval exps
            (value-of/k (car exps) env
              (list-cont '() (cdr exps) env cont))))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        ;; or (logged-print val)  ; if you use drscheme-init-cps.rkt
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
            (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))
        (cons1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (cons2-cont val saved-cont)))
        (cons2-cont (val1 saved-cont)
          (apply-cont saved-cont
            (list-val (cons val1 (expval->list val)))))
        (null?-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (null? (expval->list val)))))
        (car-cont (saved-cont)
          (apply-cont saved-cont
            (car (expval->list val))))
        (cdr-cont (saved-cont)
          (apply-cont saved-cont
            (list-val
              (cdr (expval->list val)))))
        (list-cont (rev-vals exps saved-env saved-cont)
          (let ((new-rev-vals (cons val rev-vals)))
            (if (null? exps)
              (apply-cont saved-cont
                (list-val
                  (reverse new-rev-vals)))
              (value-of/k (car exps) saved-env
                (list-cont
                  new-rev-vals
                  (cdr exps)
                  saved-env
                  saved-cont)))))
       )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))
  
  )
  


  
