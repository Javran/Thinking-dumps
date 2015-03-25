;;; explicit control evaluator with compiled-code supports

;; originally, there is an inconsistency
;; between our evaluator and compiler:
;; when a function is applied with arguments,
;; the evaluator evaluates arguments from left to right while
;; the code produces by the compiler evaluates arguments from right to left.
;; this might lead to many confusions
;; so the first problem to be solved is to make this consistent:
;; we modify the compiler so that it produces code that evaluates
;; the argument from left to right.
;; this problem is fixed and now both compiler and evaluator
;; will evaluate arguments from left to right

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
(load "simu_additive-assemble_patch.scm")
(load "simu_ec_patch.scm")
(load "exercise_5_23_common.scm")
(load "ec-plus-eval.scm")

(load "compiler.scm")
;; apply this compiler patch
;; to make argument evaluation consistent
(load "exercise_5_36_compiler.scm")

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

;; well, we are still somehow cheating, because we are allowing
;; a register to contain entire information of procedures.
;; in real life we might need some mechanism to compile
;; procedures, load compiled instruction sequences and
;; ensure we can transfer control to that instruction sequence.

(define (ec-ops-builder-modifier current-ops-builder)
  (lambda (m)
    (let* ((old-ops (current-ops-builder m))
           (new-prim-symbols
            ;; only add those that don't show up
            ;; in the old primitive list ...
            (set-union
             ;; primitive operations used in the compiler
             primitive-operations
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
      (machine-extra-set! m 'global-env env)
      (machine-fresh-start! m))))

;; TODO: compiler patch disables it .. not sure why
(define prompt-for-input display)

;; TODO: we can first compile the code,
;; and put the resulting instruction sequence
;; in front of the one that evaluator have.
;; by doing this, we can get rid of the flag hack.

(compile-and-go
 '(begin
    (define (fib n)
      (if (<= n 1)
          n
          (+ (fib (- n 1))
             (fib (- n 2)))))
    (begin
      (define x 1)
      (let ((a (begin
                 (set! x (+ x 10))
                 x))
            (b (begin
                 (set! x (* x 2))
                 x)))
        (cons a b)))))
