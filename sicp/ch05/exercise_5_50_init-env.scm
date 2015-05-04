;; evaluate a symbol using scheme evaluator
;; and the current toplevel environment
;; make an implemented language primitive entry
;; in the following format:
;; (<the symbol> <the corresponding value>)
(define (to-eval-prim-entry sym)
  `(,sym ,(lift-primitive (eval sym user-initial-environment))))

;; initialize an environment that contains basic stuff
;; for the implemented language
(define (init-env)
  (let ((proc-list
         (map to-eval-prim-entry
              '(
                + - * /
                = > >= < <=
                zero? eq? eqv?
                car cdr cons null?
                list even? odd?
                not remainder quotient
                sqrt integer?
                member memq delete
                abs append pair?
                symbol? display newline
                error set-car! set-cdr!
                number? string? char? boolean?
                list? read
                ))))
    (extend-environment
     '(true false apply-prim)
     `(#t
       #f
       ,(lift-primitive
         (lambda (proc args)
           ;; "proc" is a value held in the environment,
           ;; the compiled program calls "primitive-procedure?"
           ;; to see whether it is a primitive object (ec-prim.scm)
           ;; therefore here we need to unwrap the procedure
           ;; to do the actual function application
           (apply (primitive-implementation proc) args))))
     (extend-environment
      (map car proc-list)
      (map cadr proc-list)
      the-empty-environment))))
