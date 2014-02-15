(load "../common/utils.scm")
(load "../common/test-utils.scm")

; TODO: change everything into `analyze` structure.

; my `eval` and `apply` implementaton.

; test flag, set to #t will perform tests when components are loaded
(define *my-eval-do-test* #t)

; basic components
(load "./my-eval-handler.scm")
(load "./my-eval-data-directed.scm")
(load "./my-eval-env.scm")
(load "./my-eval-utils.scm")

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

(define (my-eval exp env)
  ; `try-xxx` are all supposed to return:
  ; either `(list <value>)` or `#f`
  (define (eval-succeeded? result)
    result)
  (define (result->val result)
    (car result))
  (define (val->result val)
    (list val))

  ; try simple form evaluation
  (define (try-simple-eval exp env)
    (cond ((self-evaluating? exp)
            (val->result exp))
          ((variable? exp)
            (val->result
              (lookup-variable-value exp env)))
          (else #f)))

  ; try to dispatch according to slot (i.e. the tag)
  (define (try-dispatch-eval exp env)
    (if (non-empty? exp)
      ; try to fetch the handler
      (let ((handler (my-eval-get (car exp))))
        (if handler
          (val->result
            (handler-eval handler exp env))
          #f))
      #f))

  ; try application
  (define (try-app-eval exp env)
    (if (application? exp)
      (val->result
        (my-apply
          (my-eval (operator exp) env)
          (list-of-values (operands exp) env)))
      #f))

  (let ((result
          (or (try-simple-eval   exp env)
              (try-dispatch-eval exp env)
              (try-app-eval      exp env))))
    (if result
      (result->val result)
      (error "unknown expression:" exp))))

;; (install-eval-quote)
;; (install-eval-define)
;; (install-eval-if)
;; (install-eval-set!)
;; (install-eval-begin)
;; (install-eval-lambda)
;; (install-eval-cond)
;; (install-eval-and)
;; (install-eval-or)
;; (install-eval-let)
;; (install-eval-let*)
;; (install-eval-letrec)
;; (install-eval-analyze)

(if *my-eval-do-test*
  (begin
    ;; (test-my-apply)
    ;; (test-init-env)
    (my-eval-test-installed-handlers)
    )
  'skipped)

(load "./my-eval-driver-loop.scm")
