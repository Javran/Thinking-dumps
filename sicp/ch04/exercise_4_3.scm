(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (eval-quotation exp env)
  (text-of-quotation exp))

(define (eval-lambda exp env)
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))

(define (eval-begin exp env)
  ; ensure the sequence is executed in the given order
  ;   dispatch to `eval-sequence`
  (eval-sequence (begin-actions exp) env))

(define (eval-cond exp env)
  ; transform `cond` into `if`
  (eval (cond->if exp) env))

; I think we need some extra codes here
;   to make sure all the handlers are defined.
; I keep the tag attached so the modification required
;   is minimized.

(put 'eval 'quote eval-quotation)
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda eval-lambda)
(put 'eval 'begin eval-begin)
(put 'eval 'cond eval-cond)

; actually we can go further to make `self-evaluating` exprs
;   and `application` exprs dispatchable.
; the idea is to set up a pair of procedure (f . g),
;   the first procedure `f` accepts an exp and tell if
;   if this is the type that its corresponding procedure `g` can handle.
;   and the one who is responsible for dispatching looks at
;   the result to determine if we call up `g` to handle it, or
;   check the next handler in list.
; this idea need to maintain a list of handlers in addition.

(define (eval exp env)
  ; the expression type should be the first element in a list
  ;   delay the computation as it will not work for some expr
  (define exp-type-delayed
    (delay (get 'eval (car exp))))

  (cond ((self-evaluating? exp)
          ; for self-evaluating expressions,
          ;   simply return itself
          exp)
        ((variable? exp)
          ; for variables, look up the corresponding
          ;   value in the environment
          (lookup-variable-value exp env))
        ((force exp-type-delayed)
          ; if this handler can be found,
          ;   then a particular `eval-xxx` is chosen
          ;   we then dispatch the rest of the list into it.
          ((force exp-type-delayed) exp env))
        ((application? exp)
          ; evaluate the first element (the operator)
          ;   and evaluate the rest part (operands) using `list-of-values`
          ;   finally call `apply` to complete the evaluation
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))

(end-script)
