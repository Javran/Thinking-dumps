(load "./ec-init-env.scm")

;; from "simu_ec_patch.scm"
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      (let* ((old-ops (old-builder m)))
        `(
          ,@(map to-machine-prim-entry primitive-operations)
          (error ,(lambda args
                    (apply error args)))
          ,@old-ops)))))

;; compile the expression
;; and run it on the machine
(define (compile-and-run-with-env exp env)
  (let* ((compiled (compile-and-check exp))
         (insn-seq (statements compiled)))
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
