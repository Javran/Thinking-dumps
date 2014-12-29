;; based on exercise 5.23
(load "./exercise_5_25_thunk.scm")

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `(,@(map to-machine-prim-entry
               '(cons cond->if let->combination normalize-define
                 delay-it thunk? thunk-exp thunk-env
                 evaluated-thunk? thunk-value thunk-set-value!))
        (cond? ,(list-tagged-with 'cond))
        (let? ,(list-tagged-with 'let))
        ,@(old-builder m)))))

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

    ;; ev-application: change to lazy evaluation
    ev-application
    (save continue)                     ; stack: [continue ..]
    (save env)                          ; stack: [env continue ..]
    (assign unev (op operands) (reg exp))
    (save unev)                        ; stack: [unev env continue ..]
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label actual-value))

    ev-appl-did-operator
    (restore unev)                      ; stack: [env continue ..]
    (restore env)                       ; stack: [continue ..]
    (assign proc (reg val))
    (assign argl (reg unev))
    ;; no longer need to evaluate them for lazy eval
    (goto (label apply-dispatch))

    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
                                        ; stack: [continue ..]
    (assign continue (label primitive-apply-after-list-args))
    (goto (label list-of-arg-values))
    primitive-apply-after-list-args
    (assign val
            (op apply-primitive-procedure)
            (reg proc)
            (reg val))
    (restore continue)
    (goto (reg continue))

    list-of-arg-values
    ;; argl -> val
    (test (op no-operands?) (reg argl))
    (branch (label list-of-arg-values-no-operand))

    ;; evaluate first argument
    (save continue)                     ; stack: [continue ..]
    (save env)                          ; stack: [env continue ..]
    (assign exp (op first-operand) (reg argl))
    (assign argl (op rest-operands) (reg argl))
    (save argl)                        ; stack: [argl env continue ..]

    (assign continue (label list-of-arg-values-first-evaluated))
    (goto (label actual-value))
    list-of-arg-values-first-evaluated
    (restore argl)                      ; stack: [env continue ..]
    (restore env)                       ; stack: [continue ..]
    (save val)                          ; stack: [val continue ..]
    (assign continue (label list-of-arg-values-rest-done))
    (goto (label list-of-arg-values))
    list-of-arg-values-rest-done
    ;; val -> exp
    (restore exp)                       ; stack: [continue ..]
    (assign val (op cons) (reg exp) (reg val))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    list-of-arg-values-no-operand
    (assign val (const ()))
    (goto (reg continue))

    compound-apply
    ;; (assign unev (op procedure-parameters) (reg proc))
    ;; (assign env (op procedure-environment) (reg proc))
    ;; (assign env
    ;;        (op extend-environment)
    ;;        (reg unev) (reg argl) (reg env))
    ;; (assign unev (op procedure-body) (reg proc))
    ;; (goto (label ev-sequence))
    (perform (op error) (const "TODO"))

    list-of-delayed-args
    ;; argl -> val
    (test (op no-operands?) (reg argl))
    (branch (label list-of-delayed-args-no-operand))

    ;; evaluate first argument
    (save continue)                     ; stack: [continue ..]
    (save env)                          ; stack: [env continue ..]
    (assign exp (op first-operand) (reg argl))
    (assign argl (op rest-operands) (reg argl))
    (save argl)                        ; stack: [argl env continue ..]

    (assign continue (label list-of-delayed-args-first-evaluated))
    (goto (label actual-value))
    list-of-delayed-args-first-evaluated
    (restore argl)                      ; stack: [env continue ..]
    (restore env)                       ; stack: [continue ..]
    (save val)                          ; stack: [val continue ..]
    (assign continue (label list-of-delayed-args-rest-done))
    (goto (label list-of-delayed-args))
    list-of-delayed-args-rest-done
    ;; val -> exp
    (restore exp)                       ; stack: [continue ..]
    (assign val (op cons) (reg exp) (reg val))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    list-of-delayed-args-no-operand
    (assign val (const ()))
    (goto (reg continue))

    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))
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

    ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))

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

    ;; actual-value exp env
    actual-value
    (save continue)                     ; stack: [continue ..]
    (assign continue (label actual-value-after1))
    ;; try to evaluate the expression, which might end up with either
    ;; a real value or a thunk
    (goto (label eval-dispatch))
    actual-value-after1
    (restore continue)                  ; stack: <balanced>
    ;; if this is a thunk, we evaluate it, which might again
    ;; end up with eitehr a real value or another thunk
    actual-value-after-eval
    (test (op thunk?) (reg val))
    (branch (label actual-value-thunk))
    (test (op evaluated-thunk?) (reg val))
    (branch (label actual-value-evaluated-thunk))
    ;; otherwise we just need to return it
    (goto (reg continue))

    (goto (reg continue))
    actual-value-thunk
    (assign exp (op thunk-exp) (reg val))
    (assign env (op thunk-env) (reg val))
    (save val)                          ; stack: [val ..]
    (save continue)                     ; stack: [continue val ..]
    (assign continue (label actual-value-thunk-after))
    (goto (label eval-dispatch))
    actual-value-thunk-after
    (restore continue)            ; stack: [val ..]
    (restore exp)                 ;; move thunk object to exp register
                                        ; stack: <balanced>
    (perform (op thunk-set-value!)
             (reg exp) (reg val))
    (goto (label actual-value-after-eval))
    actual-value-evaluated-thunk
    (assign val (op thunk-value) (reg val))
    (goto (reg continue))
    ))

;; Local variables:
;; proc-entry: "./exercise_5_25.scm"
;; End:
