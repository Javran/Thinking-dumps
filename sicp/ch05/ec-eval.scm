;; for now I'm not sure what to do
;; guess if we copy the code here,
;; this will soon become useful
(define core-dispatch
  '(eval-dispatch
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

    (goto (label unknown-expression-type))))

(define simple-expressions
  '(ev-self-eval
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
    (assign val (op make-procedure)
                (reg unev) (reg exp) (reg env))
    (goto (reg continue))))

(define proc-application
  '(ev-application
    (save continue) ; stack: [continue ..]
    (save env) ; stack: [env continue ..]
    (assign unev (op operands) (reg exp))
    (save unev) ; stack: [unev env continue ..]
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

    ;; back
    ev-appl-did-operator
    (restore unev) ; stack: [env continue ..]
    (restore env) ; stack: [continue ..]
    ;; things evaluated so far
    (assign argl (op empty-arglist))
    ;; evaluated procedure
    (assign proc (reg val))
    ;; call without operands, do application
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc) ; stack: [proc continue ..]
    ev-appl-operand-loop
    (save argl) ; stack: [argl proc continue ..]
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env) ; stack: [env argl proc continue ..]
    (save unev) ; stack: [unev env argl proc continue ..]
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ;; back
    ev-appl-accumulate-arg
    (restore unev) ; stack: [env argl proc continue ..]
    (restore env) ; stack: [argl proc continue ..]
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl) ; stack: [proc continue ..]
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc) ; stack: [continue ..]
    (goto (label apply-dispatch))
    ;; TODO: stack not balanced here?
    ))
