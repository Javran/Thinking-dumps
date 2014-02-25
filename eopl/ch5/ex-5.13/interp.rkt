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
  (provide instrument-cont)

  (define instrument-cont (make-parameter #f))

  (define (nprint who pat . args)
    (when (instrument-cont)
      (eopl:printf "~A: " who)
      (apply eopl:printf (cons pat args))
      (newline)))
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
        (const-exp (num)
          (nprint 'const-exp
            "send value ~A to continuation" num)
          (apply-cont cont (num-val num)))
        (var-exp (var)
          (nprint 'var-exp
            "send value of ~A to continuation" var)
          (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (nprint 'proc-exp
            "send procedure to continuation")
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (nprint 'letrec-exp
            "start working on body")
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (nprint 'zerp?-exp
            "start working on the operand")
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (nprint 'let-exp
            "start working on the value of variable ~A" var)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (nprint 'if-exp
            "start working on the predicate")
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (nprint 'diff-exp
            "start working on first operand")
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (mult-exp (exp1 exp2)
          (nprint 'diff-exp
            "start working on first operand")
          (value-of/k exp1 env
            (mult1-cont exp2 env cont)))
        (call-exp (rator rand) 
          (nprint 'call-exp
            "start working on the operator")
          (value-of/k rator env
            (rator-cont rand env cont)))
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
          (nprint 'zerp?-exp
            "checking if the value is zero")
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (nprint 'let-exp
            "start working on body, extend env with var: ~A"
            var)
          (value-of/k body
            (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (begin
               (nprint 'if-exp
                 "start working on the consequent part")
               (value-of/k exp2 saved-env saved-cont))
             (begin
               (nprint 'if-exp
                 "start working on the alternative part")
               (value-of/k exp3 saved-env saved-cont))))
        (diff1-cont (exp2 saved-env saved-cont)
          (nprint 'diff-exp
            "start working on second operand")
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (nprint 'diff-exp
              "yields ~A, send to continuation" (- num1 num2))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (mult1-cont (exp2 saved-env saved-cont)
          (nprint 'diff-exp
            "start working on second operand")
          (value-of/k exp2
            saved-env (mult2-cont val saved-cont)))
        (mult2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (nprint 'diff-exp
              "yields ~A, send to continuation" (* num1 num2))
            (apply-cont saved-cont
              (num-val (* num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (nprint 'call-exp
            "start working on the operand")
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (nprint 'call-exp
            "apply procedure")
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))
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
  


  
