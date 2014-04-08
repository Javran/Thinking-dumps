;;; amb evaluator, procedure application

;; evaluate all arguments in order
(define (get-args aprocs env succeed fail)
  ;; the argument list is empty, return nil (empty list)
  (if (null? aprocs)
      (succeed '() fail)
      ;; more elements waiting for evaluation
      ((car aprocs)
       ;; try evaluating first argument in the list
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         ;; keep going, recursively
         (get-args
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            ;; construct the value, and chain-call next succees cont.
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

;; TODO: need some adjustment regarding the existing system of my-eval.
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

(define (analyze-application exp)
  ;; analyze procedure and all arguments
  (let ((fproc (amb-analyze (operator exp)))
        (aprocs (map amb-analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             ;; evaluate the program
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
