(load "exercise_5_42_compiler.scm")

;; based on the function of same name in ex 5.42
(define (compile-and-run-with-env exp env)
  (let* ((compiled (compile-and-check
                    (transform-exp
                     exp)))
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
