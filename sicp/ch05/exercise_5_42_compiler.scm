(load "exercise_5_39_common.scm")
(load "exercise_5_40_compiler.scm")
(load "exercise_5_41_common.scm")

(set! primitive-operations
      (set-union primitive-operations
                 '(lexical-address-lookup
                   lexical-address-set!)))

(define (compile-variable exp target linkage ctenv)
  ;; it's guaranteed that "exp" is a variable
  ;; because it's the reason that the expression
  ;; gets dispatched here...
  (let ((ct-result (find-variable exp ctenv)))
    (end-with-linkage
     linkage
     (make-instruction-sequence
      '(env)
      (list target)
      ;; if the compile-time environment does not contain
      ;; the variable, we have to lookup runtime environment instead
      (if (equal? ct-result 'not-found)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))
          `((assign ,target (op lexical-address-lookup)
                    (const ,ct-result)
                    (reg env))))))))

(define (compile-assignment exp target linkage ctenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ctenv))
        (ct-result (find-variable exp ctenv)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       (if (equal? ct-result 'not-found)
           `((perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))
           `((perform (op lexical-address-set!)
                      (const ,ct-result)
                      (reg env)
                      (reg val))
             (assign ,target (const ok)))))))))
