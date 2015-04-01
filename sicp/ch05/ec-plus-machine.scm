;;; simulator changes and extensions

;; the "additive-assemble" is not necessary
;; for "ec-plus" to work properly
;; but it makes it possible to insert assembled
;; sequence of instructions at runtime.
;; therefore applying this patch
;; will just "do the right thing"
(load "simu_additive-assemble_patch.scm")

(load "simu_ec_patch.scm")
(load "exercise_5_23_common.scm")
(load "ec-plus-eval.scm")

(load "ec-plus-prim.scm")

;; required operations are extracted from the
;; ec machine code and also the fixed set of
;; operations used in the compiler.
(define (ec-get-required-operations)
  (set-union
   ;; fool-proof, prevent duplicated elements
   (remove-duplicates
    ;; primitive-operations copied from the compiler
    ;; as it might get mutated when loading patches.
    '(false?
      lookup-variable-value
      set-variable-value!
      define-variable!
      make-compiled-procedure
      compiled-procedure-env
      extend-environment
      list
      cons
      snoc ;; added for exercise_5_36_compiler.scm
      compiled-procedure-entry
      primitive-procedure?
      apply-primitive-procedure))
   (map car (extract-operations evaluator-insns))))

(define (ec-ops-builder-modifier current-ops-builder)
  (lambda (m)
    (let* ((old-ops (current-ops-builder m))
           (current-ops
            `((get-global-environment
               ,(lambda ()
                  (machine-extra-get m 'global-env 'error)))
              (error ,(lambda args
                        (apply error args)))
              ,@old-ops))
           (missing-prim-symbols
            (set-diff
             (ec-get-required-operations)
             (remove-duplicates
              (map car current-ops)))))
      `(
        ,@(map to-machine-prim-entry missing-prim-symbols)
        ,@current-ops))))
