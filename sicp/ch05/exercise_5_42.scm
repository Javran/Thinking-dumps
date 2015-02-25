(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
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

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; TODO: for now the test cases here are not doing much
;; except making sure things don't break
;; let's see if we want to change it when we come back...

;; TODO: tmp fix, will remove later
;; needs one more arg to compile
(define (compile-and-check exp)
  (let ((compiled (compile exp 'val 'next (empty-ctenv) )))
    (assert (check-instruction-sequence compiled)
            ;; the error message is not actually reachable
            "instruction sequence check failed.")
    compiled))

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
