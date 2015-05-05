(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define metacircular-program
  (call-with-input-file
      "metacircular-evaluator.scm"
    (lambda (p)
      (let loop ((next-result (read p)))
        (if (eof-object? next-result)
            '()
            (cons next-result (loop (read p))))))))

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_50_init-env.scm")

(let* ((compiled
        (compile-and-check
         `(begin
            ,@metacircular-program)))
       (env (init-env))
       (insn-seq (statements compiled)))
  (let ((m (build-and-execute-with
            `(controller
              ,@insn-seq)
            `((env ,env))
            machine-ops-builder)))
    (machine-reg-get m 'val)))

(define (metacirc-compile-eval exp env)
  (let* ((compiled
          (compile-and-check
           `(begin
              ,@metacircular-program
              ,exp)))
         (insn-seq (statements compiled)))
    (let ((m (build-and-execute-with
              `(controller
                ,@insn-seq)
              `((env ,env))
              machine-ops-builder)))
      (machine-reg-get m 'val))))

#;
(out (metacirc-compile-eval '((lambda (x) x) (+ 1 2 3 4)) (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
