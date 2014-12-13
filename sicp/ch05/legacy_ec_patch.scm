;; copied and modified base on simu_ec_patch.scm
(load "./ec-eval.scm")

;; evaluate a symbol under the current toplevel
;; environment, and make it an primitive entry
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

(define default-primitive-list
  (let ((old-primitive-list default-primitive-list))
    (lambda ()
      (let* ((old-ops (old-primitive-list))
             (new-prim-symbols
              (set-diff
               ec-required-operations
               (map car old-ops))))
        `(
          ,@(map to-machine-prim-entry new-prim-symbols)
          (error ,(lambda args
                    (apply error args)))
          ,@old-ops)))))

;; use the machine to evlauate a lisp expression
(define (machine-eval exp env)
  (let* ((entry-label (gensym))
         (exit-label (gensym))
         (eval-label (gensym))
         (m (make-and-execute
             `(controller
               (goto (label ,entry-label))
               ,eval-label
               ,@evaluator-insns
               ,entry-label
               (assign continue (label ,exit-label))
               (goto (label ,eval-label))
               ,exit-label)
             `((exp ,exp)
               (env ,env)))))
    (get-register-contents m 'val)))
