(load "../common/utils.scm")
(load "../common/test-utils.scm")

; my `eval` and `apply` implementaton.

; test flag, set to #t will perform tests when components are loaded
(define *my-eval-do-test* #t)

; basic components
(load "./my-eval-handler.scm")
(load "./my-eval-data-directed.scm")
(load "./my-eval-env.scm")
(load "./my-eval-utils.scm")
(load "./my-eval-maybe.scm")

; procedure support
(load "./my-eval-apply.scm")
(load "./my-eval-init-env.scm")

; basic environment

; build-in handler
(load "./my-eval-e-simple.scm")

; other handlers
; installations are delayed since `my-eval`
;   has not yet defined
(load "./my-eval-e-quote.scm")
(load "./my-eval-e-set.scm")
(load "./my-eval-e-define.scm")
(load "./my-eval-e-if.scm")
(load "./my-eval-e-begin.scm")
(load "./my-eval-e-lambda.scm")
(load "./my-eval-e-cond.scm")
(load "./my-eval-e-and.scm")
(load "./my-eval-e-or.scm")
(load "./my-eval-e-let.scm")
(load "./my-eval-e-let-star.scm")
(load "./my-eval-e-letrec.scm")
(load "./my-eval-e-analyze.scm")

; evaluation approaches
(load "./my-eval-interpret.scm")
(load "./my-eval-analyze.scm")

;; all supported eval approaches
(define eval-approaches
   (list
    (list 'interpret
          my-eval-interpret)
    (list 'analyze
          my-eval-analyze)))

; change this value according
;   to change the evaluation approach
(define *my-eval-approach*
  'interpret)

(define my-eval
  (cadr (assoc *my-eval-approach* eval-approaches)))

(install-eval-quote)
(install-eval-define)
(install-eval-if)
(install-eval-set!)
(install-eval-begin)
(install-eval-lambda)
(install-eval-cond)
(install-eval-and)
(install-eval-or)
(install-eval-let)
(install-eval-let*)
(install-eval-letrec)
(install-eval-analyze)

(if *my-eval-do-test*
  (begin
    (test-my-apply)
    (test-init-env)
    (my-eval-test-installed-handlers)
    )
  'skipped)

(load "./my-eval-driver-loop.scm")
