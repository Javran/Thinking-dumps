(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LEXADDR language.

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")
  (require (only-in racket foldl))

  (provide value-of-translation value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-translation : Nameless-program -> ExpVal

  (define value-of-translation
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))

  ;; value-of-translation : Nameless-program -> ExpVal
  ;; Page: 100
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))
  
  ;; value-of : Nameless-exp * Nameless-env -> ExpVal
  (define value-of
    (lambda (exp nameless-env)
      (cases expression exp
                (const-exp (num) (num-val num))

        (diff-exp (exp1 exp2)
          (let ((val1
		  (expval->num
		    (value-of exp1 nameless-env)))
                (val2
		  (expval->num
		    (value-of exp2 nameless-env))))
            (num-val
	      (- val1 val2))))
        
        (zero?-exp (exp1)
	        (let ((val1 (expval->num (value-of exp1 nameless-env))))
            (if (zero? val1)
              (bool-val #t)
              (bool-val #f))))

        (if-exp (exp0 exp1 exp2) 
          (if (expval->bool (value-of exp0 nameless-env))
            (value-of exp1 nameless-env)
            (value-of exp2 nameless-env)))

        (call-exp (rator rands)          
          (let ((proc (expval->proc (value-of rator nameless-env)))
                (args (map (lambda (rand) (value-of rand nameless-env))
                           rands)))
            (apply-procedure proc args)))

        (nameless-var-exp (lex-depth var-pos)
          (apply-nameless-env nameless-env lex-depth var-pos))

        (nameless-let-exp (exp1 body)
          (let ((val (value-of exp1 nameless-env)))
            (value-of body
              (extend-nameless-env val nameless-env))))

        (nameless-proc-exp (arg-count body)
          (proc-val
            (procedure arg-count body nameless-env)))

        (else
         (eopl:error 'value-of 
	    "Illegal expression in translated code: ~s" exp))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (arg-count body saved-env)
          (if (not (= (length args) arg-count))
            (eopl:error 'apply-procdure
              "arity mismatch: need ~A args, but args = ~A"
              arg-count args)
            'ok)
          (define proc-env
            (foldl
              extend-nameless-env
              saved-env
              args))
          (value-of body proc-env)))))

  )
