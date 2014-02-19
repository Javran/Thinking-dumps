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


;; TODO: `apply` and `eval` should agree
;; on the data structure, which means for
;; `eval-interpret`, we should have an `apply-interpret`
;; and the same thing apply to `eval-analyze`
;; I plan to:
;; split `my-eval-apply.scm` into two parts
;; * my-eval-apply-base.scm (basic operations)
;; * my-eval-apply-interpret.scm (support for `eval-interpret`)
;; * my-eval-apply-analyze.scm (support for `eval-analyze`) (new)

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
  'analyze)

(define my-eval
  (cadr (assoc *my-eval-approach* eval-approaches)))

;; here I have some concerns about using `my-eval` procedure
;; in the handlers' implementations,
;; as when we are running in `interpret` mode,
;; we don't have to use an analyze-based version of `my-eval`
;; (and verse visa),
;; But finally I choose to go with that
;; because of the following two reasons:
;; * `my-eval` should behavior equally well (in terms of the outcome)
;; * we can easily mutate the global variables
;;   to switch between two impls, and if there's anything wrong,
;;   one of the implementation must be wrong
;;   because we don't assume anything more than the representation
;;   of environments on the `my-eval`

(install-eval-quote)
(install-eval-define)
(install-eval-if)
(install-eval-set!)
(install-eval-begin)
(install-eval-lambda)
;; (install-eval-cond)
;; (install-eval-and)
;; (install-eval-or)
;; (install-eval-let)
;; (install-eval-let*)
;; (install-eval-letrec)
;; (install-eval-analyze)

(if *my-eval-do-test*
  (begin
    (test-my-apply)
    (test-init-env)
    (my-eval-test-installed-handlers)
    )
  'skipped)

(load "./my-eval-driver-loop.scm")
