(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; using the framework of my-eval.
;; disabling all tests for compatibility.
(define *my-eval-do-test* #f)

(load "./my-eval-handler.scm")
(load "./my-eval-data-directed.scm")
(load "./my-eval-env.scm")
(load "./my-eval-utils.scm")
(load "./my-eval-maybe.scm")

(load "./my-eval-apply.scm")
(load "./my-eval-init-env.scm")

(load "./amb-eval-analyze.scm")

(load "./amb-eval-e-lambda.scm")
(load "./amb-eval-e-if.scm")
(load "./amb-eval-e-begin.scm")
(load "./amb-eval-e-define.scm")
(load "./amb-eval-e-set.scm")

(install-amb-if)

(define (run-slot-test slot)
  (let ((handler (my-eval-get slot)))
    (out handler)
    (if (ahandler? handler)
        (ahandler-run-test handler)
        (error "no such slot" slot))))

(run-slot-test 'if)

;; TODO: try to have a complete document about what's going on here.
;; just copying the code from book won't make much sense

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       ;; try evaluating this part..
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION"
                     proc))))

;; don't know where to put it for now...
;; looks like written in CPS..
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             ;; evaluate the program
             ;; give it a suitable env to run
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(out (amb-eval `(if #f 10 20)
               (init-env)
               (lambda (exp fail) exp)
               (lambda () #f)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
