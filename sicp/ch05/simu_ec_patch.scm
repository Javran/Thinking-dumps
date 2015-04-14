(load "ec-eval.scm")
(load "ec-tests.scm")

(load "simu-monitor-patch.scm")

;; evaluate a symbol under the current toplevel
;; environment, and make it an primitive entry
;; note that the symbol must be defined before we call this function
;; or otherwise we would fail to evaluate the symbol and cause an error
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

;; note that this is an "operation list builder modifier"
;; since ec's ops-builder need to examine the current builder
;; and add missing operations, rather than offering a static list
(define (ec-ops-builder-modifier current-ops-builder)
  (lambda (m)
    (let* ((old-ops (current-ops-builder m))
           (new-prim-symbols
            ;; only add those that don't show up
            ;; in the old primitive list ...
            (set-diff (ec-get-required-operations)
                      (map car old-ops))))
      `(
        ;; we are trying to be lazy here by:
        ;; * extract the list of required operation names directly
        ;;   from the code of the evaluator
        ;; * operation names are symbols, and as we have implemented
        ;;   them somewhere in our toplevel user environment
        ;;   we can evaluate them directly to convert each operation symbol
        ;;   to its corresponding primitive entry
        ,@(map to-machine-prim-entry new-prim-symbols)
        (error ,(lambda args
                  (apply error args)))
        ,@old-ops))))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   (ec-ops-builder-modifier
    (ops-builder-union
     monitor-patch-ops-builder-extra
     default-ops-builder))))

;; use the machine to evaluate a lisp expression
;; NOTE: "machine-eval" expects a "eval-dispatch" label
;; available in "evaluator-insns", which should use "exp"
;; and "env" registers for interpreting the expression and
;; use "continue" register to transfer control.
(define (machine-eval exp env)
  (let* ((entry-label (gensym))
         (exit-label (gensym))
         (m (build-and-execute
             `(controller
               ;; set up control, assuming
               ;; "exp" and "env" are set properly
               (assign continue (label ,exit-label))
               (goto (label eval-dispatch))
               ;; instructions for the evaluator
               ,@evaluator-insns
               ;; the evaluating subroutine isn't supposed
               ;; to run past this line.
               ;; if it does, the something is wrong
               ;; with the evaluator
               (perform
                (op error)
                (const "evaluator instruction sequence error"))
               ;; if everything works fine, we should be able to
               ;; jump to here.
               ,exit-label)
             `((exp ,exp)
               (env ,env)
               (argl *unassigned*)
               (proc *unassigned*)
               ))))
    (machine-reg-get m 'val)))

(define (ec-repl)
  (build-and-execute
   `(controller
     (goto (label read-eval-print-loop-init))
     ,@evaluator-insns)
   '()))
