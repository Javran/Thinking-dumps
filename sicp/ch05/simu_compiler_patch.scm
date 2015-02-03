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
