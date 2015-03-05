(load "exercise_5_39_common.scm")
(load "exercise_5_40_compiler.scm")
(load "exercise_5_41_common.scm")

;; new primitives
(set! primitive-operations
      (set-union primitive-operations
                 '(lexical-address-lookup
                   lexical-address-set!
                   get-global-environment)))

;; overwriting default "machine-operation-builder"
;; because we need to deal with "get-global-environment"
;; differently
(define machine-ops-builder
  (let ((liftable-prims
         ;; "liftable operations"
         ;; are those that can be lifted from the
         ;; implementing language into the machine
         ;; operation
         (set-delete 'get-global-environment
                     primitive-operations)))
    (ops-builder-union
     (lambda (m)
       `(
         ;; lift "liftable" primitives
         ,@(map to-machine-prim-entry liftable-prims)
         ;; for this operation, we need to access an extra
         ;; field from the machine itself.
         ;; (we can also do this by keeping a register
         ;; which the compiler can never written to)
         (get-global-environment
          ,(lambda ()
             (machine-extra-get m 'global-env 'error)))
         (error ,(lambda args
                   (apply error args)))))
     default-ops-builder)))

(define (compile-and-run-with-env exp env)
  (let* ((compiled (compile-and-check exp))
         (insn-seq (statements compiled)))
    (let ((m (build-with
              `(controller
                ,@insn-seq)
              `((env ,env))
              machine-ops-builder)))
      ;; "env" is also recorded to the machine
      ;; so that we can retrieve that information at runtime
      (machine-extra-set! m 'global-env env)
      (machine-fresh-start! m)
      (machine-reg-get m 'val))))

(define (compile-variable exp target linkage ctenv)
  ;; it's guaranteed that "exp" is a variable
  ;; because it's the reason that the expression
  ;; gets dispatched here...
  (let ((ct-result (find-variable exp ctenv)))
    (end-with-linkage
     linkage
     (if (equal? ct-result 'not-found)
         ;; if the compile-time environment does not contain
         ;; the variable, we have to lookup runtime environment instead
         (make-instruction-sequence
          '()
          (list target 'env)
          `( (assign env (op get-global-environment))
             (assign ,target
                     (op lookup-variable-value)
                     (const ,exp)
                     (reg env))))
         (make-instruction-sequence
          '(env)
          (list target)
          ;; if the compile-time environment does not contain
          ;; the variable, we have to lookup runtime environment instead
          `((assign ,target (op lexical-address-lookup)
                        (const ,ct-result)
                        (reg env))))))))

(define (compile-assignment exp target linkage ctenv)
  (let* ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ctenv))
        (ct-result (find-variable var ctenv)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (if (equal? ct-result 'not-found)
          (make-instruction-sequence
           '(env val)
           (list target 'env)
           `((assign env (op get-global-environment))
             (perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok))))
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op lexical-address-set!)
                      (const ,ct-result)
                      (reg env)
                      (reg val))
             (assign ,target (const ok)))))))))

;; Local variables:
;; proc-entry: "exercise_5_42.scm"
;; End:
