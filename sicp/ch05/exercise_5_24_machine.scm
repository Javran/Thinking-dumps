(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      ;; preserving the modifications we have for
      ;; "let" and "define"
      `(,(to-machine-prim-entry 'let->combination)
        ,(to-machine-prim-entry 'normalize-define)
        (cond? ,(list-tagged-with 'cond))
        (let? ,(list-tagged-with 'let))
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
    ;; TODO:
    ;; I think the biggest issue here
    ;; is not about how to implement it
    ;; but which register should I use and how to use it
    ;; and also I don't get it why we must jump to ev-sequence.
    ;; isn't ev-sequence one part of ev-begin subroutine?
    ;; what are the assumptions that ev-sequence made?
    ;; nothing but confusion here.

    ;; store the list of condition clauses in "exp"
    (assign exp (op cond-clauses) (reg exp))

    ;; if the clause list is empty,
    ;; we return #f
    (test (op null?) (reg exp))
    (branch (label ev-cond-empty-clause))

    ;; now we have a non-empty list of clauses to work with ...

    ;; save the list of causes on the stack
    (save exp) ;; stack: [exp ..]
    ;; extract condition
    (assign exp (op first-clause) (reg exp))

    (save exp) ;; stack: [first-clause exp ..]
    ;; TODO: need to check if this is a symbol: else

    (test (op else-clause?) (reg exp))
    (branch (label ev-cond-else-clause))

    (assign exp (op car) (reg-exp))

    (save continue) ;; stack: [continue first-clause exp ..]
    (assign continue (label ev-cond-eval-cond-continue))
    (save env) ;; stack: [env continue first-clause exp ..]
    (goto (label eval-dispatch))
    ev-cond-eval-cond-continue
    (restore env) ;; stack: [continue first-clause exp ..]
    (restore continue) ;; stack: [first-clause exp ..]
    (test (op true?) (reg val))
    (branch (label ev-cond-sub-eval))


    ;; if there's no clause inside "cond"
    ;; we return #f
    ev-cond-empty-clause
    (assign val (const #f))
    (goto (reg continue))

    ;; the condition is evaluated to true
    ;; now we evaluate the following sequence ...
    ev-cond-sub-eval
    ;; TODO

    ;; "(else <a seq of exp>)"
    ev-cond-else-clause


    (perform (op error) (const "TODO"))

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
;; proc-entry: "./exercise_5_23.scm"
;; End:
