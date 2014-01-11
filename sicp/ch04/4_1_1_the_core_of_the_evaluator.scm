(load "../common/utils.scm")
(load "../common/test-utils.scm")

; evaluate an expression in a given environment
(define (eval exp env)
  (cond ((self-evaluating? exp)
          ; for self-evaluating expressions,
          ;   simply return itself
          exp)
        ((variable? exp)
          ; for variables, look up the corresponding
          ;   value in the environment
          (lookup-variable-value exp env))
        ((quoted? exp)
          ; for quoted expressions,
          ;   return the quoted expression
          (text-of-quotation exp))
        ((assignment? exp)
          ; dispatch to `eval-assignment`
          (eval-assignment exp env))
        ((definition? exp)
          ; dispatch to `eval-definition`
          (eval-definition exp env))
        ((if? exp)
          ; dispatch to `eval-if`
          (eval-if exp env))
        ((lambda? exo)
          ; build the procedure using
          ;   parameters and the body
          (make-procedure
            (lambda-parameters exp)
            (lambda-body exp)
            env))
        ((begin? exp)
          ; ensure the sequence is executed in the given order
          ;   dispatch to `eval-sequence`
          (eval-sequence (begin-actions exp) env))
        ((cond? exp)
          ; transform `cond` into `if`
          (eval (cond->if exp) env))
        ((application? exp)
          ; evaluate the first element (the operator)
          ;   and evaluate the rest part (operands) using `list-of-values`
          ;   finally call `apply` to complete the evaluation
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
          ; dispatch, if the procedure is already a primitive
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          ; sequentially evluating the body
          ;   using extended environment
          ;   (the procedure environment + parameter bindings)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  ; evaluate each expression
  ;   in environment `env` and return a list of result
  (if (no-opearnds? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  ; use `true?` here allows the language to be implemented
  ;   having a different representaion of truth value
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
          ; return the result of the last expresssion
          (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

; just came with an idea of refactoring
#|
(define (eval-sequence exps env)
  (let ((result (eval (first-exp exps) env)))
    (if (not (last-exp? exps))
      (eval-sequence (rest-exps exps) env)
      result)))
|#

(define (eval-assignment exp env)
  ; update the environment
  (set-variable-value!
    (assignment-variable exp)
    ; evaluate and bind to the variable
    (eval (assignment-value exp) env)
    env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp))
    env)
  'ok)

; missing definitions:
; * define-variable!
; * definition-variable
; * definition-value
; * set-variable-value!
; * assignment-value
; * assignment-variable
; * rest-exps
; * first-exp
; * last-exp?
; * if-consequent
; * if-alternative
; * if-predicate
; * true?
; * rest-operands
; * first-operand
; * no-opearnds?
; * self-evaluating?
; * variable?
; * lookup-variable-value
; * quoted?
; * text-of-quotation
; * assignment?
; * definition?
; * if?
; * lambda?
; * make-procedure
; * lambda-parameters
; * lambda-body
; * begin?
; * begin-actions
; * cond?
; * cond->if
; * application?
; * apply
; * operator
; * operands
; * primitive-procedure?
; * apply-primitive-procedure
; * compound-procedure?
; * procedure-body
; * extend-environment
; * procedure-parameters
; * procedure-environment

(end-script)
