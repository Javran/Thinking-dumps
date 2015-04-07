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

(assert
 (equal?
  (compile-then-interp-with-env
   ;; case: target == val && linkage == return
   ;; This happens when the compiled procedure
   ;;   does a tail-recursive call to the compound procedure.
   ;;   this call passed the "return" linkage to the compound
   ;;   procedure and requires the compound procedure
   ;;   to place the result on "val" register
   '(define (test-apply f)
      (f 10))
   '(test-apply (lambda (x) (+ 5 x)))
   (init-env))
  ;; (+ 5 10) ==> 15
  15))

(assert
 (equal?
  (compile-then-interp-with-env
   ;; case: target == val && linkage /= return
   ;; This happens when the compiled procedure
   ;;   needs a compound procedure to give it a result,
   ;;   but there are still remaining tasks for
   ;;   this compiled procedure to do.
   ;;   therefore the compound procedure
   ;;   need to use linkage other than "return"
   ;;   to direct back
   '(define (test-apply f)
      (+ (f 100) 123))
   '(test-apply (lambda (x) (* x x)))
   (init-env))
  ;; (+ (* 100 100) 123)
  10123))

(assert
 (equal?
  (compile-then-interp-with-env
   ;; case: target /= val && linkage /= return
   ;; We know there is one case that target is not
   ;;   "val" register: when we are doing function application,
   ;;   the result actually goes to "proc" register.
   ;;   This means we need to have a compound procedure
   ;;   at the operator position. After evaluating the operator
   ;;   we still need to worry about operands (even if
   ;;   there is none, we still need to give "argl" register
   ;;   some value), so the linkage is unlikely to be "return".
   ;;   On the other hand, linkage == return
   ;;   violates the convention because if linkage == return,
   ;;   we are supposed to place the result in "val" register.
   '(begin
      (define (g x) (* 5 x))
      (define (test-apply f)
        ((f g) 7)))
   '(test-apply (lambda (x) x))
   (init-env))
  ;; (* 5 7)
  35))

(compile-and-go
 '(begin
    (define (identity x) x)
    ))
