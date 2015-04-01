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
             (set-union
              primitive-operations
              (ec-get-required-operations))
             (remove-duplicates
              (map car current-ops)))))
      `(
        ,@(map to-machine-prim-entry missing-prim-symbols)
        ,@current-ops))))
