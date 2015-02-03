;; some extra functionalities for the compiler
(load "./simu.scm")
(load "./ec-init-env.scm")

(load "./simu_compiler_patch.scm")

(define (print-instruction-sequence insn-seq)
  (format #t "Registers needed: ~A~%~
              Registers modified: ~A~%~
              ;;;; Instruction listing:~%"
          (registers-needed insn-seq)
          (registers-modified insn-seq))
  (for-each (lambda (insn)
              (format #t "  ~A~%" insn))
            (statements insn-seq))
  (out ";;;; End of listing"))

;; TODO: actually we know which set of operations
;; we will be using,
;; lift them beforehands,
;; instead of lifting them on the fly

;; TODO: checkings should be done only once,
;; since it's compiled it doesn't make sense
;; running checkings on a static data multiple times
;; isolate this part of functionalities

;; TODO: decouple compiler with the machine

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
;; proc-entry: "./compiler-tests.scm"
;; End:
