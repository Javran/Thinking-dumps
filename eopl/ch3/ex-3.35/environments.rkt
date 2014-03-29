(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.rkt. 

  (require "data-structures.rkt")
  (require (only-in racket vector-ref))

  (provide init-env empty-env extend-env apply-env)

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

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (var val saved-env)
          ; we can tell the enviroment is created
          ;   by `extend-env` or `extend-env-rec`
          ;   by examining the data type of `val`
          (if (vector? val)
            ; extend-env-rec
            ; follow the specification
            (if (eqv? search-sym var)
              ; return the proc
              (vector-ref val 0)
              ; else
              (apply-env saved-env search-sym))
            ; extend-env
            (if (eqv? search-sym var)
              val
              (apply-env saved-env search-sym)))))))
  )
