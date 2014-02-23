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

;; evaluation approaches
;; notice that the implementation of `apply-<strategy>`
;; is now in the following corresponding files
(load "./my-eval-interpret.scm")
(load "./my-eval-analyze.scm")

;; previously I said `apply` should agree with `eval`
;; but maybe I was wrong:
;; the thing is that in analyze-strategy, we should
;; use another data structure to represent a procedure
;; rather than using `make-procedure`, which essentially
;; causes `apply` disagree with the structure of `proc-compound`.

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
  'uninitialized)

(define my-eval
  'uninitialized)

(define (my-eval-select-approach approach)
  (my-eval-message
   "switching to approach: ~A" approach)
  (set! *my-eval-approach* approach)
  (set! my-eval
        (cadr (assoc *my-eval-approach* eval-approaches))))

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

;; we have some problem with this implementation,
;; and I think we shouldn't keep `eval` as a global variable
;; for each handlers to use.
;; When I design this system, it only assumes support for
;; `interpret` mode, and as the `analyze` mode comes,
;; this approach becomes only a `workaround` despite that
;; it sounds not something really bad.

;; the biggest problem here is:
;; we can only have one "eval-session",
;; i.e. we cannot keep two "eval" instances living
;; without mutating the global variable
;; (i.e. `*my-eval-approach*` and `my-eval`)

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

(if *my-eval-do-test*
    ;; test all approaches
    (for-each
     (lambda (approach)
       (begin
         (my-eval-select-approach approach)
         (test-my-apply)
         (test-init-env)
         (my-eval-test-installed-handlers)
         ))
     (map car eval-approaches))
    'skipped)

(load "./my-eval-driver-loop.scm")
