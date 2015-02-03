(load "./ec-init-env.scm")

;; from "simu_ec_patch.scm"
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      (let* ((old-ops (old-builder m))
             (new-prim-symbols
              '(
                ;; by searching "(op <something>)" from compiler's source code
                ;; we are able to extract all possible primitives that
                ;; our compiled code might use.
                false?
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
                apply-primitive-procedure)))
        `(
          ,@(map to-machine-prim-entry new-prim-symbols)
          (error ,(lambda args
                    (apply error args)))
          ,@old-ops)))))

;; TODO: checkings should be done only once,
;; since it's compiled it doesn't make sense
;; running checkings on a static data multiple times
;; isolate this part of functionalities

;; compile the expression
;; and run it on the machine
(define (compile-and-run-with-env exp env)
  (let* ((compiled (compile exp 'val 'next))
         (insn-seq (statements compiled)))
    ;; check register requirement
    (let ((needed (registers-needed compiled)))
      (assert (or (null? needed)
                  (equal? needed '(env)))
              "the only required register (if any) should be 'env'"))
    ;; verify labels
    (if (check-labels insn-seq)
        'ok
        ;; not actually reachable
        (out "Error regarding labels occurred."))
    (let ((m (build-and-execute
              `(controller
                ,@insn-seq)
              `((env ,env)))))
      (machine-reg-get m 'val))))

(define (compile-and-run exp)
  (compile-and-run-with-env exp (init-env)))

;; Local variables:
;; proc-entry: "./simu_compiler_patch_tests.scm"
;; End:
