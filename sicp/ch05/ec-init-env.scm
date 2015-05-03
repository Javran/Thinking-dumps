;; evaluate a symbol using scheme evaluator
;; and the current toplevel environment
;; make an implemented language primitive entry
;; in the following format:
;; (<the symbol> <the corresponding value>)
(define (to-eval-prim-entry sym)
  `(,sym ,(lift-primitive (eval sym user-initial-environment))))

;; TODO:
;; undo changes made by ex 5.50.
;; as it should keep its own set of patches

#;
(define (apply-meta proc args)
  (define proc-prim (compose car   proc-fields))
  (apply (proc-prim proc) args))

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
                ;; TODO: justify them? -- split it.
                error
                set-car! set-cdr!
                number? string? char? boolean?
                list?
                read
                ))))
    (extend-environment
     ;; TODO: remove hacking
     '(true false magic-lift)
     `(#t   #f  ,(lift-primitive
                  (lambda (sym)
                    (environment-lookup user-initial-environment sym))   )
            )
     (extend-environment
      (map car proc-list)
      (map cadr proc-list)
      the-empty-environment))))
