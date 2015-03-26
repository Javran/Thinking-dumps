;;; explicit control evaluator with compiled-code supports

;; see document in "ec-plus.md" for more details

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
;; this is actually not necessary -- TODO: more details
(load "simu_additive-assemble_patch.scm")
(load "simu_ec_patch.scm")
(load "exercise_5_23_common.scm")
(load "ec-plus-eval.scm")

(load "compiler.scm")

;; apply this compiler patch
;; to make argument evaluation consistent
;;(check document for more details)
(load "exercise_5_36_compiler.scm")

;; TODO: fix syntax errors in the document
;; TODO: too many comments, let's create a text file
;; TODO: break procedures into meaningful modules

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

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps)
(newline)

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
         ;; there should not be conflicting labels.
         ;; on one hand, our implementation of "simu.scm" can detect
         ;; duplicated labels automatically, so if it happens to be the case
         ;; that a compiled label conflicts with one used in our evaluator
         ;; our "assemble" procedure will reject to proceed.
         ;; on the other hand, we have carefully designed our evaluator
         ;; so that labels does not conflict and our compiler keeps a counter
         ;; itself and guarantees to generate unique labels. With these two
         ;; facts together, it's safe to say that our labels don't conflict.
         (m (build-with
             `(controller
               (goto (label external-entry))
               ,@evaluator-insns
               ;; we will always append extra code to the tail
               ;; of the previous instruction sequence
               ;; so that the behavior is consistent with
               ;; "additive assemble" patch.
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
