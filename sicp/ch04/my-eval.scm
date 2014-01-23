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

; build-in handler
(load "./my-eval-e-simple.scm")

; other handlers
; installations are delayed since `my-eval`
;   has not yet defined
(load "./my-eval-e-quote.scm")
(load "./my-eval-e-set.scm")
(load "./my-eval-e-define.scm")
(load "./my-eval-e-if.scm")

; TODO:
; * lambda
; * begin
; * cond
; * application

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

(install-eval-quote)
(install-eval-define)
(install-eval-if)
(install-eval-set!)

(if *my-eval-do-test*
  (my-eval-test-installed-handlers)
  'skipped)
