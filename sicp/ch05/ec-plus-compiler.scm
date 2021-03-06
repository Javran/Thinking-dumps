;;; compiler-related changes and extensions

;; apply this compiler patch
;; to make argument evaluation consistent
;; (check document for more details)
(load "exercise_5_36_compiler.scm")
(load "simu_compiler_patch.scm")

;; as we are importing "simu_compiler_patch.scm",
;; we'd better tell it to use our new ops builder modifier
(define (compile-and-run-with-env exp env)
  (let* ((compiled (compile-and-check exp))
         (insn-seq (statements compiled)))
    (let ((m (build-and-execute-with
              `(controller
                ,@insn-seq)
              `((env ,env))
              (ec-ops-builder-modifier
               (ops-builder-union
                monitor-patch-ops-builder-extra
                default-ops-builder)))))
      (machine-reg-get m 'val))))

;; based on the one with the same name in
;; "compiler-verify.scm"
(define (check-instruction-sequence compiled-seq)
  (let ((insn-seq (statements compiled-seq))
        (needed (registers-needed compiled-seq)))
    (assert
     (set-subset<=? needed '(env))
     "the only required register (if any) should be 'env'")
    (if (check-labels insn-seq)
        'ok
        (out "Error regarding labels occurred."))

    (let ((operations (map car (extract-operations insn-seq))))
      (assert (set-subset<=? (remove-duplicates operations)
                             ;; primitive operations are just part of
                             ;; required operations
                             (ec-get-required-operations))
              "unknown operation found"))
    #t))

(define (compile-and-check exp)
  (let ((compiled (compile exp 'val 'next)))
    (assert (check-instruction-sequence compiled)
            ;; the error message is not actually reachable
            "instruction sequence check failed.")
    compiled))

;; as we have mentioned in the comment of our "assemble" procedure:
;; our implementation doesn't add instruction sequences together
;; and only the last one assembled takes effect.
;; there are two possible solutions:
;;
;; * additive "assemble" procedure: I think it's already too late
;;   to go back and make big changes to "assemble" to make it additive
;;   so we can simply create a new "assemble-additive" procedure
;;   that preserves the old instruction sequence and appends
;;   the newly assembled one right after it.
;; * we build the instruction sequence before creating the machine,
;;   and we make sure to only assemble the instruction sequence once.
;;   but later, we might need to figure out how can we do compilation,
;;   assemble and insert newly compiled instructions into the machine
;;   at run time.

(define (compile-and-go exp)
  (let* ((compiled
          (compile exp 'val 'return))
         (insn-seq (statements compiled))
         (env (init-env))
         (m (build-with
             `(controller
               (goto (label external-entry))
               ,@evaluator-insns
               ;; we will always append extra code to the tail
               ;; of the previous instruction sequence
               ;; so that the behavior is consistent with
               ;; "additive assemble" patch.
               ;; i.e. new codes are always attached
               ;; to the existing one.
               external-entry
               (perform (op initialize-stack))
               (assign env (op get-global-environment))
               (assign continue (label print-result))
               ,@insn-seq
               )
             `((env ,env))
             (ec-ops-builder-modifier
              (ops-builder-union
               monitor-patch-ops-builder-extra
               default-ops-builder)))))
    (machine-extra-set! m 'global-env env)
    (machine-fresh-start! m)))
