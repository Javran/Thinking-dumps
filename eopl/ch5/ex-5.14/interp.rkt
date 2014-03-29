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

  (define (ndep depth)
    (when (instrument-cont)
      (eopl:printf "cont depth: ~A~%" depth)))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont) 1)))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont dep)
      (cases expression exp
        (const-exp (num)
          (apply-cont cont (num-val num) dep))
        (var-exp (var)
          (apply-cont cont (apply-env env var) dep))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))
            dep))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont dep))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont) (+ dep 1)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)
            (+ dep 1)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)
            (+ dep 1)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)
            (+ dep 1)))
        (mult-exp (exp1 exp2)
          (value-of/k exp1 env
            (mult1-cont exp2 env cont)
            (+ dep 1)))
        (call-exp (rator rand) 
          (ndep dep)
          (value-of/k rator env
            (rator-cont rand env cont)
            (+ dep 1)))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val dep)
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
              (zero? (expval->num val)))
            dep))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
            (extend-env var val saved-env) saved-cont dep))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (begin
               (value-of/k exp2 saved-env saved-cont dep))
             (begin
               (value-of/k exp3 saved-env saved-cont dep))))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont) dep))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2))
              dep)))
        (mult1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (mult2-cont val saved-cont) dep))
        (mult2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (* num1 num2))
              dep)))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont) dep))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont dep)))
        )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont dep)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont
            dep)))))
  
  )
  


  
