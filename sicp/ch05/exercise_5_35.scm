(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")

(for-each
 out
 (statements
  (compile-and-check
   `(define (f x)
      (+ x (g (+ x 2)))))))

;; copied from the exercise, not exactly the same
;; because of the labels.
(define compiled-code
  '(
    (assign val (op make-compiled-procedure) (label entry16) (reg env))
    ;; * it's a lambda definition
    (goto (label after-lambda15))
    entry16
    (assign env (op compiled-procedure-env) (reg proc))
    ;; argument list is  "(x)", i.e. "(define (f x) ...)"
    (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
    ;; (+ ...)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (save continue)
    (save proc)
    (save env)
    ;; (+ ... (g ...))
    (assign proc (op lookup-variable-value) (const g) (reg env))
    (save proc)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    ;; (+ ... (g ... (+ ...)))
    (assign val (const 2))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const x) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    ;; (+ ... (g ... (+ x 2)))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch19))
    compiled-branch18
    (assign continue (label after-call17))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch19
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call17
    (assign argl (op list) (reg val))
    ;; (+ ... (g (+ x 2)))
    ;; popping out "g"
    (restore proc)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch22))
    compiled-branch21
    (assign continue (label after-call20))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch22
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call20
    (assign argl (op list) (reg val))
    (restore env)
    ;; (+ ... x (g (+ x 2)))
    (assign val (op lookup-variable-value) (const x) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (restore continue)
    ;; (+ x (g (+ x 2)))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch25))
    compiled-branch24
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch25
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call23
    after-lambda15
    (perform (op define-variable!) (const f) (reg val) (reg env))
    (assign val (const ok))
    ))
