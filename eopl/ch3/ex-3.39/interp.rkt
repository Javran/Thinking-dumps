(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LEXADDR language.

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")

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

        (cons-exp (exp1 exp2)
          (let ((expval1 (value-of exp1 nameless-env))
                (expval2 (value-of exp2 nameless-env)))
            (list-val
              (cons expval1 (expval->list expval2)))))

        (car-exp (exp1)
          (car (expval->list (value-of exp1 nameless-env))))

        (cdr-exp (exp1)
          (list-val (cdr (expval->list (value-of exp1 nameless-env)))))

        (null?-exp (exp1)
          (bool-val (null? (expval->list (value-of exp1 nameless-env)))))

        (empty-list-exp ()
          (list-val '()))

        (call-exp (rator rand)          
          (let ((proc (expval->proc (value-of rator nameless-env)))
                (arg (value-of rand nameless-env)))
            (apply-procedure proc arg)))

        (nameless-var-exp (n)
          (apply-nameless-env nameless-env n))

        (nameless-let-exp (exp1 body)
          (let ((val (value-of exp1 nameless-env)))
            (value-of body
              (extend-nameless-env val nameless-env))))

        (nameless-proc-exp (body)
          (proc-val
            (procedure body nameless-env)))

        (else
         (eopl:error 'value-of 
	    "Illegal expression in translated code: ~s" exp))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (body saved-env)
          (value-of body (extend-nameless-env arg saved-env))))))

  )
