(define (my-analyze exp)
  ;; a simple form is either a self-evaluating s-exp
  ;; or a variable that needs to be looked up from the env
  (define (try-simple-analyze exp)
    (cond ((self-evaluating? exp)
           (just (const exp)))
          ((variable? exp)
           (just
            (lambda (env)
              (lookup-variable-value exp env))))
          (else nothing)))

  ;; try to dispatch the s-exp according to slots
  (define (try-dispatch-analyze exp)
    (if (non-empty? exp)
        ;; try to fetch the handler
        (let ((handler (my-eval-get (car exp))))
          (if handler
              (just
               (handler-analyze handler exp))
              nothing))
        nothing))

  ;; TODO: my-apply should have different impl as well.
  ;; try application
  (define (try-app-analyze exp)
    (if (application? exp)
        (just
         ;; analyze everything in the s-exp
         (let ((rator (my-analyze (operator exp)))
               (rands (map my-analyze (operands exp))))
           (lambda (env)
             ;; provide each exp with an `env`
             (my-apply
              (rator env)
              (map (lambda (rand) (rand env))
                   rands)))))
        nothing))

  ((maybe
    ;; if the analysis goes well, return the result
    identity
    ; else there must be some errors
    (lambda ()
      (error "unknown expression:" exp)))
   (or (try-simple-analyze exp)
       (try-dispatch-analyze exp)
       (try-app-analyze exp)
       nothing)))

(define (my-apply-analyze proc args)
  (cond ((proc-primitive? proc)
         (apply-proc-primitive proc args))
        ((proc-compound? proc)
         (apply-proc-compound proc args))
        
        (else
         (error
          "Unknown procedure type: APPLY" proc))))

(define (my-apply-interpret proc args)
  (define apply-proc
    (cond ((proc-primitive? proc)
            apply-proc-primitive)
          ((proc-compound? proc)
            apply-proc-compound)
          (else
            (error
              "Unknown procedure type: APPLY" proc))))
  (apply-proc proc args))
;; an analyze-xxx is a procedure of form `(analyze-xxx exp)`
;; this procedure turns an analyzer into a `(eval-xxx exp env)`
(define (analyze->eval analyze-xxx)
  (define (eval-xxx exp env)
    ((analyze-xxx exp) env))
  eval-xxx)

(define my-eval-analyze
  (analyze->eval my-analyze))
;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:
