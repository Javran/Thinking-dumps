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

(load "ec-tests.scm")
;; make sure to load the new definition of "init-env" after "ec-tests.scm"
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

;; approach can take either 'interpret or 'analyze
(define (metacirc-compile-eval-with-approach approach)
  (lambda (exp env)
    (let* ((compiled
            (compile-and-check
             `(begin
                ,@metacircular-program
                (my-eval-select-approach (quote ,approach))
                ,exp)))
           (insn-seq (statements compiled)))
      (let ((m (build-and-execute-with
                `(controller
                  ,@insn-seq)
                `((env ,env))
                machine-ops-builder)))
        (machine-reg-get m 'val)))))

;; note that these tests will take a while,
;; since we are compiling and running the metacircular evaluator
;; on the virtual machine we have for each test case.
(for-each
 (test-evaluator (metacirc-compile-eval-with-approach 'interpret))
 test-exps)

(for-each
 (test-evaluator (metacirc-compile-eval-with-approach 'analyze))
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
