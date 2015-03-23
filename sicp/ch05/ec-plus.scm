;;; explicit control evaluator with compiled-code supports

;; there is an inconsistency between our evaluator and compiler:
;; when a function is applied with arguments,
;; the evaluator evaluates arguments from left to right while
;; the code produces by the compiler evaluates arguments from right to left.
;; this might lead to many confusions
;; so the first problem to be solved is to make this consistent:
;; we modify the compiler so that it produces code that evaluates
;; the argument from left to right.

;; to verify the order of argument evaluation,
;; we can use the following expression:

#|
(begin
  (define x 1)
  (let ((a (begin
             (set! x (+ x 10))
             x))
        (b (begin
             (set! x (* x 2))
             x)))
    (cons a b)))
|#

;; the result will be:
;; * (11 . 22) if arguments are evaluated from left to right
;; * (12 . 2)  if arguments are evaluated from right to left

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "simu_ec_patch.scm")
(load "exercise_5_23_common.scm")
(load "ec-plus-eval.scm")

(load "compiler.scm")

(define (user-print . args)
  (define (user-print-one obj)
    (cond ((compound-procedure? obj)
           (out (list 'compound-procedure
                      (procedure-parameters obj)
                      (procedure-body obj)
                      '<procedure-env>)))
          ((compiled-procedure? obj)
           (out '<compiled-procedure>))
          (else (out obj))))
  (for-each user-print-one args))

#;(load "ec-tests.scm")
#;(for-each
 (test-evaluator machine-eval)
 test-exps)
#;(newline)

;; TODO: I think the primitive operations used in evaluator
;; should be a superset of those used in a compiler,
;; maybe we can just use those primitives in the evaluator

;; well, we are still somehow cheating, because we are allowing
;; a register to contain entire information of procedures.
;; in real life we might need some mechanism to compile
;; procedures, load compiled instruction sequences and
;; ensure we can transfer control to that instruction sequence.


;; TODO: check compiled code as well?
#;
(out
 (if (check-labels evaluator-insns)
     "no problem with the label checker"
     "some missing labels found"))

(define (ec-ops-builder-modifier current-ops-builder)
  (lambda (m)
    (let* ((old-ops (current-ops-builder m))
           (new-prim-symbols
            ;; only add those that don't show up
            ;; in the old primitive list ...
            (set-union
             '(make-compiled-procedure
               compiled-procedure?
               compiled-procedure-env
               compiled-procedure-entry
               ;; TODO: need to figure out a better way...
               false?
               list
               cons)
             (set-delete
              'get-global-environment
              (set-diff (ec-get-required-operations)
                        (map car old-ops))))))
      `(
        ;; we are trying to be lazy here by:
        ;; * extract the list of required operation names direcly
        ;;   from the code of the evaluator
        ;; * operation names are symbols, and as we have implemented
        ;;   them somewhere in our toplevel user environment
        ;;   we can evaluate them directly to convert each operation symbol
        ;;   to its corresponding primitive entry
        ,@(map to-machine-prim-entry new-prim-symbols)
        (get-global-environment
         ,(lambda ()
            (machine-extra-get m 'global-env 'error)))
        (error ,(lambda args
                  (apply error args)))
        ,@old-ops))))

(define (compile-and-go exp)
  (let* ((compiled (compile-and-check exp))
         (insn-seq (statements compiled))
         (env (init-env)))
    (let ((m (build-with
              `(controller
                (goto (label read-eval-print-loop-init))
                ,@evaluator-insns)
              `((env ,env)
                (flag #t))
              (ec-ops-builder-modifier
               (ops-builder-union
                monitor-patch-ops-builder-extra
                default-ops-builder)))))
      (machine-reg-set! m 'val (assemble insn-seq m))
      ;; redo assembling here...
      ;; TODO: still not working
      (assemble
       `(controller
         (goto (label read-eval-print-loop-init))
         ,@evaluator-insns)
       m)
      (machine-extra-set! m 'global-env env)
      (machine-fresh-start! m))))

;; TODO: compiler patch disables it .. not sure why
(define prompt-for-input display)

(compile-and-go
 '(define (fib n)
    (if (<= n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))
