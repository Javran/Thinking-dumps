;; merging changes from ex 5.23 for better syntax support

(load "./ec-eval_v2.scm")
(load "./ec-tests.scm")

(load "./simu-monitor-patch.scm")
(load "./exercise_5_23_common.scm")

;; evaluate a symbol under the current toplevel
;; environment, and make it an primitive entry
;; note that the symbol must be defined before we call this function
;; or otherwise we would fail to evaluate the symbol and cause an error
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

;; TODO: the method we used isn't satisfactory
;; as we have failed to reuse some obvious duplicate codes.
;; the difference is only in the instruction list
;; and we should take advantage of it.
;;
;; a potential plan:
;; * add support for "updating existing primitive operations"
;; * when the instruction list gets updated, we rerun a certain
;;   procedure to update the default operation builder
;; * we can make "ec-required-operations" a procedure
;;   to delay its calculation - which might be out of sync
;;   when we change the code
(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      (let* ((old-ops (old-builder m))
             (new-prim-symbols
              ;; only add those that don't show up
              ;; in the old primitive list ...
              (set-diff ec-required-operations
                        (map car old-ops))))
        `(
          ;; we are trying to be lazy here by:
          ;; * extract the list of required operation names direcly
          ;;   from the code of the evaluator
          ;; * operation names are symbols, and as we have implemented
          ;;   them somewhere in our toplevel user environment
          ;;   we can evaluate them directly to convert each operation symbol
          ;;   to its corresponding primitive entry
          ,@(map to-machine-prim-entry new-prim-symbols)
          (error ,(lambda args
                    (apply error args)))
          ,@old-ops)))))

;; use the machine to evlauate a lisp expression
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
