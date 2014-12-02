;; for now I'm not sure what to do
;; guess if we copy the code here,
;; this will soon become useful

;; TODO: need to extract a list of operations from the instruction list

(define evaluator-insns
  '(
    ;; ==== eval-dispatch
    ;; input: exp env
    ;; output: val
    eval-dispatch
    ;; all the following subroutines will return by
    ;; jumping to "continue".
    ;; therefore no explicit return in this subroutine
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

    (test (op application?) (reg exp))
    (branch (label ev-application))

    (goto (label unknown-expression-type))
    ;; ==== ev-self-eval
    ;; input: exp
    ;; output: val
    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))

    ;; ==== ev-variable
    ;; input: exp env
    ;; output: val
    ev-variable
    (assign
     val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))

    ;; ==== ev-quoted
    ;; input: exp
    ;; output: val
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))

    ;; ==== ev-lambda
    ;; input: exp env
    ;; output: val
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val
            (op make-procedure)
            (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ;; ==== ev-application
    ;; input: exp env
    ;; output: val
    ev-application
    (save continue)                     ; stack: [continue ..]
    (save env)                          ; stack: [env continue ..]
    (assign unev (op operands) (reg exp))
    (save unev)                        ; stack: [unev env continue ..]
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    ;; exp -> val
    (goto (label eval-dispatch))

    ev-appl-did-operator
    (restore unev)                      ; stack: [env continue ..]
    (restore env)                       ; stack: [continue ..]
    ;; things evaluated so far
    (assign argl (op empty-arglist))
    ;; evaluated procedure
    (assign proc (reg val))
    ;; procedure called without operands, do application
    (test (op no-operands?) (reg unev))
    ;; note here "continue" is not popped from the stack
    (branch (label apply-dispatch))
    ;; otherwise we need to evaluate them all
    (save proc)                         ; stack: [proc continue ..]
    ev-appl-operand-loop
    (save argl)                       ; stack: [argl proc continue ..]
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    ;; this is not the last arg
    (save env)               ; stack: [env argl proc continue ..]
    (save unev)              ; stack: [unev env argl proc continue ..]
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ev-appl-accumulate-arg
    ;; first operand turned into val,
    ;; add it to argl
    (restore unev)                ; stack: [env argl proc continue ..]
    (restore env)                 ; stack: [argl proc continue ..]
    (restore argl)                ; stack: [proc continue ..]
    ;; TODO: note that if we insert "val" in front of "argl"
    ;; then "argl" is storing arguments in the reversed order,
    ;; what is "adjoin-arg" is not mentioned in the book
    ;; but I guess it needs to at least keep the order of "argl"
    ;; and the order of "unev" consistent.
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    ;; drop the first operand
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ;; we only go to here when there's only one unevaluated expression left
    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)                      ; stack: [proc continue ..]
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)                      ; stack: [continue ..]
    (goto (label apply-dispatch))
    ;; TODO: stack not balanced here?
    ;; note that when calling "apply-dispatch",
    ;; a "continue" is always on the stack.. but why?
    ;; ---looks like apply-dispatch simply assume that
    ;; there's always one "continue" on the top of the stack
    ;; --- I guess this is for the same reason as "eval-dispatch"
    ;; being explicit on the idea that we are doing tail-recursion

    ;; ==== apply-dispatch
    ;; input: proc argl
    ;; expecting a "continue" on the top of the stack
    ;; output: val
    apply-dispatch
    (test (op primitve-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign val
            (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (restore continue) ; stack: <balanced>
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env
            (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

    ;; ==== ev-begin
    ;; input: exp env
    ;; output: val
    ev-begin
    ;; TODO: what's begin-actions?
    (assign unev (op begin-actions) (reg exp))
    (save continue)                     ; stack: [continue ..]
    (goto (label ev-sequence))
    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    ;; seems like every time we call a "eval-dispatch" subroutine
    ;; the preparation phase looks similiar
    (save unev)                        ; stack: [unev continue ..]
    (save env)                         ; stack: [env unev continue ..]
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    eq-sequence-continue
    (restore env)                       ; stack: [unev continue ..]
    (restore unev)                      ; stack: [continue ..]
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)                  ; stack: <balanced>
    ;; transfer directly to eval-dispatch without saving
    ;; any information on the stack
    (goto (label eval-dispatch))

    ;; ==== ev-if
    ;; input: exp env
    ;; output: val
    (save exp)                          ; stack: [exp ..]
    (save env)                          ; stack: [env exp ..]
    (save continue)                     ; stack: [continue env exp ..]
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))        ; evaluate the predicate
    ev-if-decide
    (restore continue)             ; stack: [env exp ..]
    (restore env)                  ; stack: [exp ..]
    (restore exp)                  ; stack: <balanced>
    ;; dispatch according to the result
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ;; ==== ev-assignment
    ;; input: exp env
    ;; output: val=ok, env modified
    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev) ; stack: [unev ..]
    (assign exp (op assignment-value) (reg exp))
    (save env) ; stack: [env unev ..]
    (save continue) ; stack: [continue env unev ..]
    (assign continue (label ev-assignment-1))
    ;; evaluate the assignment value
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue) ; stack: [env unev ..]
    (restore env) ; stack: [unev ..]
    (restore unev) ; stack: <balanced>
    (perform
     (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ;; ==== ev-definition
    ;; input: exp env
    ;; output: val=ok, env modified
    (assign unev (op definition-variable) (reg exp))
    (save unev) ; stack: [unev ..]
    (assign exp (op definition-value) (reg exp))
    (save env) ; stack: [env unev ..]
    (save continue) ; stack: [continue env unev ..]
    (assign continue (label ev-definition-1))
    ev-definition-1
    (restore continue) ; stack: [env unev ..]
    (restore env) ; stack: [unev ..]
    (restore unev) ; stack: <balanced>
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))
    ))
