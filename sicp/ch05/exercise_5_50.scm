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

(let* ((compiled
        (compile-and-check
         `(begin
            ,@metacircular-program)))
       (env (init-env))
       (insn-seq (statements compiled)))
  (out "compiled")
  (let ((m (build-and-execute-with
            `(controller
              ,@insn-seq)
            `((env ,env))
            machine-ops-builder)))
    (machine-reg-get m 'val)))

;; TODO: note: looks like we can use --stack to get rid of
;; stack limit, but somehow the memory still brow up...

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
