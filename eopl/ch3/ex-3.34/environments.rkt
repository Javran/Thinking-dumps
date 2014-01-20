(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.rkt. 

  (require "data-structures.rkt")

  (provide init-env empty-env extend-env extend-env-rec apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

    ;: * empty-env
    ;; * extend-env
    ;; * extend-env-rec
    ;; * apply-env
    ;; * environment?

    ;; procedural representaion structure:
    ;; 3 forms:
    ;; * (list 'environment 'empty <query-env>)
    ;; * (list 'environment 'extend <query-env>)
    ;; * (list 'environment 'extend-rec <query-env>)
    ;; query-env: Env x Var -> ExpVal

    ;; because of cycling dependency,
    ;; some functions are moved to "./data-structures.rkt"

    (define (apply-env env search-sym)
      (define query-env
        (caddr env))
      (query-env env search-sym))

    (define (empty-env)
      (define (query-env env search-sym)
        (eopl:error 'apply-env "No binding for ~s" search-sym))
      (list 'environment 'empty query-env))

    (define (extend-env var val saved-env)
      (define (query-env env search-sym)
        (if (eqv? search-sym var)
          val
          (apply-env saved-env search-sym)))
      (list 'environment 'extend query-env))

    (define (extend-env-rec p-name b-var p-body saved-env)
      (define (query-env env search-sym)
        (if (eqv? search-sym p-name)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-sym)))
      (list 'environment 'extend-rec query-env))
  )
