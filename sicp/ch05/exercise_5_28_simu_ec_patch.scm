;; it sounds really stupid to have multiple files
;; that looks almost the same.
;; Believe me, I also hate it.

(load "./exercise_5_28_ec-eval.scm")
(load "./ec-tests.scm")

(load "./simu-monitor-patch.scm")
(load "./exercise_5_23_common.scm")

(define no-more-exps? null?)

(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      (let* ((old-ops (old-builder m))
             (new-prim-symbols
              (set-diff ec-required-operations
                        (map car old-ops))))
        `(,@(map to-machine-prim-entry new-prim-symbols)
          (error ,(lambda args
                    (apply error args)))
          ,@old-ops)))))

(define (machine-eval exp env)
  (let* ((entry-label (gensym))
         (exit-label (gensym))
         (eval-label (gensym))
         (m (build-and-execute
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
    (machine-reg-get m 'val)))

(define (ec-repl)
  (build-and-execute
   `(controller
     (goto (label read-eval-print-loop-init))
     ,@evaluator-insns)
   '()))
