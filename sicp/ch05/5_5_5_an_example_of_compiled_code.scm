;; an example of complied code

;; the following code is used
;; for generating the instruction sequence.
#|
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")

(for-each
 out
 (statements
  (compile-and-check
   `(define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n))))))
|#

;; comments are numbered from outer expressions to inner ones
(define compiled-factorial
  '( ;; code generated from compiling the lisp expression
    ;; 1. the expression is a definition
    ;;   therefore the definition body gets evaluated first
    ;;   the definition body is a lambda-expression,
    ;;   whose resulting sequence of instructions is attached
    ;;   right afer the value assignment.
    (assign val (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))
    ;; 3. the entry for the compiled lambda-exprssion
    ;; this is the part compiled from:
    ;;   (lambda (n)
    ;;     (if (= n 1)
    ;;       1
    ;;       (* (factorial (- n 1)) n)))
    ;;
    entry2
    ;; pop out embeded environment from the procedure structure
    (assign env (op compiled-procedure-env) (reg proc))
    ;; extend the environment with argument bindings
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
    ;; preserving "continue" "env"
    ;; (= n 1)
    (save continue)                     ; stack: [continue ...]
    (save env)                          ; stack: [env continue ...]
    (assign proc (op lookup-variable-value) (const =) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch17))
    compiled-branch16
    (assign continue (label after-call15))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch17
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call15
    (restore env)                       ; stack: [continue ...]
    (restore continue)                  ; stack: <balanced>
    ;; (= n 1) stored in "val"
    ;; 4. the if-expression
    (test (op false?) (reg val))
    (branch (label false-branch4))
    ;; 1
    true-branch5
    (assign val (const 1))
    (goto (reg continue))
    ;; (* (factorial (- n 1)) n )
    false-branch4
    (assign proc (op lookup-variable-value) (const *) (reg env))
    (save continue)                     ; stack: [continue ...]
    (save proc)                         ; stack: [proc continue ...]
    ;; n (second argument to "*")
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op list) (reg val))
    (save argl)                      ; stack: [argl proc continue ...]
    ;; (factorial (- n 1))
    (assign proc (op lookup-variable-value) (const factorial) (reg env))
    (save proc)                 ; stack: [proc argl proc continue ...]
    ;; (- n 1)
    (assign proc (op lookup-variable-value) (const -) (reg env))
    ;; 1
    (assign val (const 1))
    (assign argl (op list) (reg val))
    ;; n
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch8))
    compiled-branch7
    (assign continue (label after-call6))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch8
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call6
    (assign argl (op list) (reg val))
    (restore proc)                   ; stack: [argl proc continue ...]
    ;; factorial
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch11))
    compiled-branch10
    (assign continue (label after-call9))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch11
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call9
    (restore argl)                      ; stack: [proc continue ...]
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)                      ; stack: [continue ...]
    (restore continue)                  ; stack: <balanced>
    ;; "*"
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch14))
    compiled-branch13
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch14
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call12
    after-if3
    ;; 2. after the evaluation of the body
    ;;   the symbol is bound to the corresponding value
    ;;   and the return value is set to "ok"
    after-lambda1
    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))))

;; Local variables:
;; proc-entry: ""
;; End:
