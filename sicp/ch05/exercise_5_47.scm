(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_eval.scm")
(load "exercise_5_47_compiler.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps) (newline)

;; some testcases might fail when the compilation
;; is not fully working, this is desired because
;; at compile time we cannot check the object and
;; make the final decision so we must take all the cases
;; into account, the compilation of compound procedure application
;; is one of these cases.
(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps) (newline)

;; first compile "compile-exp", load it into machine
;; and then interpret "interp-exp"
;; returns whatever stored in "val" register
(define (compile-then-interp-with-env
         compile-exp
         interp-exp
         env)
  ;; based on compile-and-go
  (let* ((compiled (compile compile-exp 'val 'return))
         (insn-seq (statements compiled))
         (m (build-with
             `(controller
               (goto (label external-entry))
               ,@evaluator-insns
               external-entry
               (assign compapp (label compound-apply))
               (perform (op initialize-stack))
               (assign env (op get-global-environment))
               ;; evaluate compiled instructions,
               ;; then proceed to handle interp-exp
               (assign continue (label after-eval-compiled))
               ,@insn-seq
               after-eval-compiled
               ;; interpret "interp-exp"
               ;; as if it was a user input
               (perform (op initialize-stack))
               (assign exp (const ,interp-exp))
               (assign env (op get-global-environment))
               ;; but stop when the evaluation is done
               (assign continue (label after-everything))
               (goto (label eval-dispatch))
               after-everything)
             `((env ,env))
             (ec-ops-builder-modifier
              (ops-builder-union
               monitor-patch-ops-builder-extra
               default-ops-builder)))))
    (machine-extra-set! m 'global-env env)
    (machine-fresh-start! m)
    (machine-reg-get m 'val)))

(out
 (compile-then-interp-with-env
  '(define (test-apply f)
     (f 10))
  '(test-apply (lambda (x) x))
  (init-env)))

;; TODO: need to summarize 3 cases.
;; TODO: console-free testcases?
(compile-and-go
 '(begin
    ;; for triggering target == val && linkage == return
    ;; use: (test-apply (lambda (x) x))
    (define (test-apply f)
      (f 10))
    ;; for triggering target == val && linkage /= return
    ;; use: (test-apply2 (lambda (x) x))
    ;; some extra stuff after function application
    ;; so that the linkage is not "return"
    (define (test-apply2 f)
      (+ (f 10) 20))

    ;; for triggering target /= val && linkage /= return
    ;; use: (test-apply3 (lambda (x) x))
    ;; "(f identity)" has to be compound procedure if "f"
    ;; is given in the interpreter, therefore the target of
    ;; compiling "(f identity)" goes to "proc" /= "val"
    ;; as we need to apply arguments after it, the linkage
    ;; cannot be "return"
    (define (identity x) x)
    (define (test-apply3 f)
      ((f identity) 10))
    ))
