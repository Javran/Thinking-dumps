(load "../common/utils.scm")
(load "../common/test-utils.scm")

; the idea:
;   the inefficiency is caused by
;   interleaving the evaluation with
;   the syntactic analysis
;   while the analysis can actually be done
;   individually.
;
; solution:
;   try to do the syntactic analysis
;   only once.
;   let another procedure to do the analysis
;   and return "the execution procedure"

; the framework will be like:
;   (define (eval exp env)
;     ((analyze exp) env))

(define (analyze-self-evaluating exp)
  ; for self-evaluating expressions,
  ;   we wrap the result inside a function
  ;   and when this function be called with
  ;   an environment, this function returns `exp`
  (lambda (env) exp))

(define (analyze-quoted exp)
  ; fetch text of quotation, same way as before
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  ; first one that uses the given env
  (lambda (env)
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        ; notice here we use recursive call
        ;   to analyze this portion of exp
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  ; since we don't know what the outcome of
  ;   pproc wiil be, we do analyze on both branch
  ;   this will not be an issue because every expression
  ;   will only get analyzed once.
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  ; note that this function accepts
  ;   a sequence of expressions rather than
  ;   a single expression.
  (define (sequentially proc1 proc2)
    ; run proc1, and then run proc2
    ;   both proc1 and proc2 are analyzed procs
    (lambda (env)
      (proc1 env)
      (proc2 env)))
  (define (loop first-proc rest-procs)
    ; loop through the procs
    (if (null? rest-procs)
        ; for the last expression,
        ;   we also return its value
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    ; break down procedures into analyezed components
    (if (null? procs)
        ; let's consider an empty sequence an error
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type:
                 EXECUTE-APPLICATION"
                 proc))))          
         
(end-script)
