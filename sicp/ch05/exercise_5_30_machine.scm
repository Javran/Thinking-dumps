;; based on "./ec-eval_v2.scm"

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

    (test (op application?) (reg exp))
    (branch (label ev-application))

    (goto (label unknown-expression-type))

    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))

    ev-variable
    (assign
     val (op lookup-variable-value) (reg exp) (reg env))
    (test (op error?) (reg val))
    (branch (label signal-error))
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
    (save continue)                     ; stack: [continue ..]
    (save env)                          ; stack: [env continue ..]
    (assign unev (op operands) (reg exp))
    (save unev)                        ; stack: [unev env continue ..]
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

    ev-appl-did-operator
    (restore unev)                      ; stack: [env continue ..]
    (restore env)                       ; stack: [continue ..]
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)                         ; stack: [proc continue ..]
    ev-appl-operand-loop
    (save argl)                       ; stack: [argl proc continue ..]
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))

    (save env)               ; stack: [env argl proc continue ..]
    (save unev)              ; stack: [unev env argl proc continue ..]
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ev-appl-accumulate-arg

    (restore unev)                ; stack: [env argl proc continue ..]
    (restore env)                 ; stack: [argl proc continue ..]
    (restore argl)                ; stack: [proc continue ..]

    (assign argl (op adjoin-arg) (reg val) (reg argl))

    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)                      ; stack: [proc continue ..]
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)                      ; stack: [continue ..]
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
    (test (op error?) (reg val))
    (branch (label signal-error))
    (restore continue)                  ; stack: <balanced>
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
    (save continue)                     ; stack: [continue ..]
    (goto (label ev-sequence))
    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))

    (save unev)                        ; stack: [unev continue ..]
    (save env)                         ; stack: [env unev continue ..]
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)                       ; stack: [unev continue ..]
    (restore unev)                      ; stack: [continue ..]
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)                  ; stack: <balanced>
    (goto (label eval-dispatch))

    ev-if
    (save exp)                          ; stack: [exp ..]
    (save env)                          ; stack: [env exp ..]
    (save continue)                     ; stack: [continue env exp ..]
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))        ; evaluate the predicate
    ev-if-decide
    (restore continue)                  ; stack: [env exp ..]
    (restore env)                       ; stack: [exp ..]
    (restore exp)                       ; stack: <balanced>

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
    (save unev)                         ; stack: [unev ..]
    (assign exp (op assignment-value) (reg exp))
    (save env)                         ; stack: [env unev ..]
    (save continue)                    ; stack: [continue env unev ..]
    (assign continue (label ev-assignment-1))

    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)                  ; stack: [env unev ..]
    (restore env)                       ; stack: [unev ..]
    (restore unev)                      ; stack: <balanced>
    (assign val
     (op set-variable-value!) (reg unev) (reg val) (reg env))
    (test (op error?) (reg val))
    (branch (label signal-error))
    (assign val (const ok))
    (goto (reg continue))

    ev-definition
    (assign exp (op normalize-define) (reg exp))
    (assign unev (op definition-variable) (reg exp))
    (save unev)                         ; stack: [unev ..]
    (assign exp (op definition-value) (reg exp))
    (save env)                         ; stack: [env unev ..]
    (save continue)                    ; stack: [continue env unev ..]
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)                  ; stack: [env unev ..]
    (restore env)                       ; stack: [unev ..]
    (restore unev)                      ; stack: <balanced>
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ev-cond
    (assign exp (op cond->if) (reg exp))

    (goto (label eval-dispatch))

    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))

    read-eval-print-loop-init
    (assign env (op init-env))

    read-eval-print-loop
    (perform (op initialize-stack))
    (perform
     (op prompt-for-input) (const "ec-repl> "))
    (assign exp (op read))
    (assign continue (label print-result))
    (goto (label eval-dispatch))

    print-result
    (perform (op print) (reg val))
    (perform (op print-stack-statistics))
    (goto (label read-eval-print-loop))

    unknown-expression-type
    (assign val (op make-error)
            (const unknown-expression-type-error)
            (reg exp))
    (goto (label signal-error))

    unknown-procedure-type
    ;; I don't think we need to take care about the stack
    ;; because the stack gets cleared whenever an error is signalled
    ;; (restore continue)
    (assign val (op make-error) (const unknown-procedure-type-error)
            (reg proc))
    (goto (label signal-error))

    signal-error
    (perform (op print) (const "error signalled:"))
    (assign val (op error-info) (reg val))
    (perform (op print) (reg val))
    (goto (label read-eval-print-loop-init))
    ))
