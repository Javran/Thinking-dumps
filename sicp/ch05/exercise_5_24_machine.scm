(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      ;; preserving the modifications we have for
      ;; "let" and "define"
      `(,(to-machine-prim-entry 'let->combination)
        ,(to-machine-prim-entry 'normalize-define)
        (cond? ,(list-tagged-with 'cond))
        (let? ,(list-tagged-with 'let))

        ,(to-machine-prim-entry 'cond-clauses)
        ,(to-machine-prim-entry 'first-clause)
        ,(to-machine-prim-entry 'rest-clauses)
        ,(to-machine-prim-entry 'clause-cond)
        ,(to-machine-prim-entry 'clause-actions)
        ,(to-machine-prim-entry 'single-clause?)
        ,(to-machine-prim-entry 'null?)
        ,(to-machine-prim-entry 'eq?)

        ,@(old-builder m)))))

;; based on exercise 5.23
(define evaluator-insns
  '(
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))

    (test (op variable?) (reg exp))
    (branch (label ev-variable))

    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))

    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))

    (test (op definition?) (reg exp))
    (branch (label ev-definition))

    (test (op if?) (reg exp))
    (branch (label ev-if))

    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))

    (test (op begin?) (reg exp))
    (branch (label ev-begin))

    (test (op cond?) (reg exp))
    (branch (label ev-cond))

    (test (op let?) (reg exp))
    (branch (label ev-let))

    ;; IMPORTANT: function application is the last resort
    ;; and should be the last case to be considered
    (test (op application?) (reg exp))
    (branch (label ev-application))

    (goto (label unknown-expression-type))

    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))

    ev-variable
    (assign
     val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))

    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))

    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val
            (op make-procedure)
            (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

    ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))

    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign val
            (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (restore continue)
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env
            (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    ;; just wondering: is the following line necessary?
    ;; I don't think so.
    ;; maybe the only point here is that ev-begin
    ;; is tail-recursive and the author want to make this explicit
    (goto (label ev-sequence))
    ;; what I hate about ev-sequence
    ;; is that it mutates "exp", the caller might still need to use it!
    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))

    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)

    (goto (label eval-dispatch))

    ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))
    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)

    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ev-definition
    (assign exp (op normalize-define) (reg exp))
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ;; ev-cond:
    ;; input: exp env
    ;; output: val
    ev-cond
    ;; use "argl" as the list of cond-clauses
    (assign argl (op cond-clauses) (reg exp))

    ev-cond-clause-test-loop
    (test (op null?) (reg argl))
    (branch (label ev-cond-no-more-clause))

    ;; put first clause to "unev"
    (assign unev (op first-clause) (reg argl))

    ;; extract condition
    (assign exp (op clause-cond) (reg unev))
    (test (op eq?) (reg exp) (const else))
    (branch (label ev-cond-exec-action))
    ;; elsewise the condition part is just a normal expression
    ;; let's evaluate it
    (save continue) ;; stack: [continue ..]
    (save argl) ;; stack: [argl continue ..]
    (save unev) ;; stack: [unev argl continue ..]
    (save env) ;; stack: [env unev argl continue ..]
    (assign continue (label ev-cond-evaluated))
    (goto (label eval-dispatch))
    ev-cond-evaluated
    (restore env) ;; stack: [unev argl continue ..]
    (restore unev) ;; stack: [argl continue ..]
    (restore argl) ;; stack: [continue ..]
    (restore continue) ;; stack: <balanced>
    (test (op true?) (reg val))
    (branch (label ev-cond-exec-action))
    ;; otherwise we need to evaluate the next clause ..
    (assign argl (op rest-clauses) (reg argl))
    (goto (label ev-cond-clause-test-loop))
    (perform (op error) (const "TODO"))

    ev-cond-no-more-clause
    (assign val (const #f))
    (goto (reg continue))

    ev-cond-exec-action
    (assign unev (op clause-actions) (reg unev))
    (save continue)
    (goto (label ev-sequence))

    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))

    unknown-expression-type
    (perform (op error)
             (const "unknown expression type")
             (reg exp))

    unknown-procedure-type
    (perform (op error)
             (const "unknown procedure type")
             (reg proc))
    ))

;; Local variables:
;; proc-entry: "./exercise_5_24.scm"
;; End:
