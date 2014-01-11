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

; missing definitions:
; * self-evaluating?
; * variable?
; * lookup-variable-value
; * quoted?
; * text-of-quotation
; * assignment?
; * eval-assignment
; * definition?
; * eval-definition
; * if?
; * eval-if
; * lambda?
; * make-procedure
; * lambda-parameters
; * lambda-body
; * begin?
; * eval-sequence
; * begin-actions
; * cond?
; * cond->if
; * application?
; * apply
; * operator
; * operands
; * list-of-values
; * primitive-procedure?
; * apply-primitive-procedure
; * compound-procedure?
; * procedure-body
; * extend-environment
; * procedure-parameters
; * procedure-environment

(end-script)
