;; based on ex 5.47
(define evaluator-insns
  '(
    ;; to make "evaluator-insns" worth its name
    ;; "eval-dispatch" subroutine need to exist.
    eval-dispatch
    (assign val (op magic-compile) (reg exp))
    (goto (reg val))

    ;; these are just tmp hacks
    ;; and we need to get rid of it
    compound-apply
    (assign argl (reg argl))
    (assign proc (reg proc))
    (assign compapp (reg compapp))
    (perform (op error) (const "impossible"))

    ;; TODO: label name
    ;; RCEPL for:
    ;; Read-Compile-Execute-Print-Loop
    read-eval-print-loop
    ;; read
    (perform
     (op prompt-for-input) (const "ec-repl+> "))
    (assign exp (op read))

    ;; compile
    ;; NOTE: because our "compile" operation
    ;; is available at the level of machine operation
    ;; when things fail to compile, it will simply
    ;; be a machine failure. (we can still do
    ;; better than this, because as a machine operation
    ;; we still have the whole machine object available.
    ;; but converting error signals to RCEPL level
    ;; might need more assumptions.)
    (assign val (op magic-compile) (reg exp))

    ;; execute
    ;; since compiling doesn't any register except "exp"
    ;; to be set, we can delay all initialization tasks
    ;; to here.
    (perform (op initialize-stack))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (reg val))

    ;; print
    print-result
    (perform (op user-print) (reg val))
    (perform (op print-stack-statistics))
    (goto (label read-eval-print-loop))

    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))

    unknown-procedure-type
    (restore continue)
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))

    signal-error
    (perform (op print) (const "error signaled:"))
    (perform (op print) (reg val))
    (goto (label read-eval-print-loop))

    ;; external-entry can be set up afterwards,
    ;; and we are not going to handle it here.
    ))
