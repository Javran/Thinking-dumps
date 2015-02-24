(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_39_common.scm")
(load "exercise_5_40_compiler.scm")
(load "exercise_5_41_common.scm")

(define primitive-operations
  '(false?
    lookup-variable-value
    set-variable-value!
    define-variable!
    make-compiled-procedure
    compiled-procedure-env
    extend-environment
    list
    cons
    compiled-procedure-entry
    primitive-procedure?
    apply-primitive-procedure
    lexical-address-lookup
    lexical-address-set!
    ))

(define (compile-variable exp target linkage ctenv)
  ;; it's guaranteed that "exp" is a variable
  ;; because it's the reason that the expression
  ;; gets dispatched here...
  (let ((ct-result (find-variable exp ctenv)))
    (if (equal? ct-result 'not-found)
        ;; fallback to lookup runtime environment if not found
        (end-with-linkage
         linkage
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))))
        ;; or use lexical addressing directly
        (end-with-linkage
         linkage
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign ,target (op lexical-address-lookup)
                            (const ,ct-result)
                            (reg env))))))))

(define (compile-assignment exp target linkage ctenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ctenv))
        (ct-result (find-variable exp ctenv)))
    (if (equal? ct-result 'not-found)
        (end-with-linkage
         linkage
         (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok))))))
        (end-with-linkage
         linkage
         (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op lexical-address-set!)
                      (const ,ct-result)
                      (reg env)
                      (reg val))
             (assign ,target (const ok)))))))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
