;; a standalone version of metacircular evaluator
;; TODO: we should better have a list of assumed
;; operations somewhere

;; NOTE: Our basic implementation will not support
;; quasiquotes. I guess this is one of the reason
;; why SICP rarely talks about quasiquotes when
;; implementing the metacircular evaluator,
;; as we need to write down something we can easily
;; implement.

(define (compose . procs)
  (define (compose-inv procs)
    (if (null? procs)
        identity
        (lambda (x)
          ((compose-inv (cdr procs)) ((car procs) x)))))
  (let ((procs-inv (reverse! procs)))
    (compose-inv procs-inv)))
(define nil '())
(define non-empty? pair?)
(define (identity x) x)
(define (const x)
  (lambda (y) x))

(define cadr
  (compose car cdr))

(define (length xs)
  (if (null? xs)
      0
      (+ (length (cdr xs)) 1)))

(define (assert val reason)
  (if val
      'ok
      (error reason)))

(define gensym
  ;; procedure-local counter
  (let ((symbol-counter 0))
    (lambda ()
      (let ((old-sym symbol-counter))
        (set! symbol-counter (+ symbol-counter 1))
        (string->symbol
         ;; we make a long name so there is
         ;; less chance of name confliction
         (string-append
          "gensym-generated-symbol#"
          (number->string old-sym)))))))

(define out
  (lambda items
    (for-each
     (lambda (x)
       (display x)
       (newline))
     items)))

(define (make-handler
         slot
         proc-eval
         proc-analyze)
  (list 'handler
        slot
        proc-eval
        proc-analyze))

(define (handler? h)
  (and (list? h)
       (not (null? h))
       (eq? (car h) 'handler)))

(define handler-slot cadr)
(define handler-proc-eval caddr)
(define handler-proc-analyze cadddr)

(define (handler-eval handler exp env)
  ((handler-proc-eval handler) exp env))

(define (handler-analyze handler exp)
  ((handler-proc-analyze handler) exp))

(define (handler-register! h)
  (my-eval-put! (handler-slot h) h))

(define my-eval-get #f)
(define my-eval-put! #f)
(define my-eval-get-all-slot-names #f)

(let ((eval-handler-alist '()))
  (define (put-handler slot handler alist)
    (let ((slot-list (assq slot alist)))
      (cons (cons (list slot handler)
                  (del-assq slot alist))
            (if slot-list
                (cadr slot-list)
                #f))))

  (define (put! slot handler)
    (let ((modified
           (put-handler slot handler eval-handler-alist)))
      (set! eval-handler-alist (car modified))
      (cdr modified)))

  (define (get-handler slot alist)
    (let ((slot-list (assq slot alist)))
      (if slot-list
          (cadr slot-list)
          #f)))

  (define (get slot)
    (get-handler slot eval-handler-alist))

  (set! my-eval-get get)
  (set! my-eval-put! put!)
  (set! my-eval-get-all-slot-names
        (lambda ()
          (map car eval-handler-alist)))
  'ok)

(define the-empty-environment nil)
(define empty-environment?    null?)
(define first-frame           car)
(define enclosing-environment cdr)

(define (make-frame variables values)
  (cons variables values))
(define frame-variables car)
(define frame-values    cdr)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))
  'ok)

(define (extend-environment vars vals base-env)
  (assert (= (length vars)
             (length vals))
          "Variable-Value list length mismatch")
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values    frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values    frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (assert (not (empty-environment? env))
          "define-variable! cannot work on an empty environment")
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values    frame))))

(define (list-tagged-with tag)
  (lambda (l)
    (and
     (list? l)
     (non-empty? l)
     (eq? (car l) tag))))

(define (tagged-list? exp tag)
  ((list-tagged-with tag) exp))

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (just a)
  (cons 'just a))

(define (just? maybe)
  (and (non-empty? maybe)
       (eq? 'just (car maybe))))

(define nothing #f)

(define from-just cdr)

(define (maybe success-f failure-f)
  (lambda (a-maybe)
    (if (just? a-maybe)
        (success-f (from-just a-maybe))
        (failure-f))))

(define (proc? proc)
  (and (list? proc)
       (> (length proc) 2)
       (eq? (car proc) 'proc)))

(define (proc-primitive? proc)
  (and (proc? proc)
       (eq? (cadr proc) 'primitive)))

(define (proc-compound? proc)
  (and (proc? proc)
       (eq? (cadr proc) 'compound)))

(define (proc-analyzed-compound? proc)
  (and (proc? proc)
       (eq? (cadr proc) 'analyzed-compound)))

(define (make-proc-primitive prim)
  (list 'proc 'primitive prim))

(define (make-procedure vars body env)
  (list 'proc 'compound vars body env))

(define (make-analyzed-procedure vars abody env)
  (list 'proc 'analyzed-compound vars abody env))

(define proc-fields cddr)

(define proc-prim (compose car   proc-fields))

(define proc-vars (compose car   proc-fields))
(define proc-body (compose cadr  proc-fields))
(define proc-env  (compose caddr proc-fields))

(define proc-a-vars proc-vars)
(define proc-a-body proc-body)
(define proc-a-env  proc-env)

(define (application? exp)
  (and (list? exp)
       (non-empty? exp)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-application rator-exp rand-exps)
  (cons rator-exp rand-exps))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (apply-proc-primitive proc args)
  (apply (proc-prim proc) args))

(define (apply-proc-compound proc args)
  (define vars (proc-vars proc))
  (define body (proc-body proc))
  (define env  (proc-env  proc))

  (my-eval
   (make-begin body)
   (extend-environment vars args env)))

(define (apply-proc-analyzed-compound proc args)
  (define vars  (proc-a-vars proc))
  (define abody (proc-a-body proc))
  (define env   (proc-a-env  proc))

  (define body-env
    (extend-environment vars args env))
  (abody body-env))

(define (my-apply proc args)
  (define apply-proc
    (cond ((proc-primitive? proc)
           apply-proc-primitive)
          ((proc-compound? proc)
           apply-proc-compound)
          ((proc-analyzed-compound? proc)
           apply-proc-analyzed-compound)
          (else
           (error
            "Unknown procedure type: APPLY" proc))))
  (apply-proc proc args))

(define (lift-primitive-pair sym)
  (cons sym
        (make-proc-primitive
         (environment-lookup
          user-initial-environment sym))))

(define (init-env)
  (let ((proc-list
         (list
          (lift-primitive-pair '+)
          (lift-primitive-pair '-)
          (lift-primitive-pair '*)
          (lift-primitive-pair '/)
          (lift-primitive-pair '=)
          (lift-primitive-pair '>)
          (lift-primitive-pair '>=)
          (lift-primitive-pair '<)
          (lift-primitive-pair '<=)
          (lift-primitive-pair 'zero?)
          (lift-primitive-pair 'eq?)
          (lift-primitive-pair 'eqv?)
          (lift-primitive-pair 'car)
          (lift-primitive-pair 'cdr)
          (lift-primitive-pair 'cons)
          (lift-primitive-pair 'null?)
          (lift-primitive-pair 'list)
          (lift-primitive-pair 'even?)
          (lift-primitive-pair 'odd?)
          (lift-primitive-pair 'not)
          (lift-primitive-pair 'remainder)
          (lift-primitive-pair 'quotient)
          (lift-primitive-pair 'sqrt)
          (lift-primitive-pair 'integer?)
          (lift-primitive-pair 'member)
          (lift-primitive-pair 'memq)
          (lift-primitive-pair 'delete)
          (lift-primitive-pair 'abs)
          (lift-primitive-pair 'append)
          )))
    (extend-environment
     '(true false)
     '(#t   #f)
     (extend-environment
      (map car proc-list)
      (map cdr proc-list)
      the-empty-environment))))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)
      (boolean? exp)))

(define variable? symbol?)

(define quoted?
  (list-tagged-with 'quote))

(define text-of-quotation cadr)

(define (install-eval-quote)
  (define (eval-quote exp env)
    (text-of-quotation exp))
  (define (analyze-quote exp)
    (let ((result (text-of-quotation exp)))
      (const result)))
  (define handler
    (make-handler
     'quote
     eval-quote
     analyze-quote))

  (handler-register! handler)
  'ok)

(define assignment-variable cadr)
(define assignment-value   caddr)

(define (install-eval-set!)
  (define (eval-set! exp env)
    (set-variable-value!
     (assignment-variable exp)
     (my-eval (assignment-value exp) env)
     env)
    'ok)
  (define (analyze-set! exp)
    (let ((var (assignment-variable exp))
          (vproc (my-analyze (assignment-value exp))))
      (lambda (env)
        (set-variable-value!
         var
         (vproc env)
         env)
        'ok)))

  (define handler
    (make-handler
     'set!
     eval-set!
     analyze-set!))

  (handler-register! handler)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (install-eval-define)

  (define (eval-define exp env)
    (define-variable!
      (definition-variable exp)
      (my-eval (definition-value exp) env)
      env)
    'ok)

  (define (analyze-define exp)
    (let ((var (definition-variable exp))
          (vproc (my-analyze (definition-value exp))))
      (lambda (env)
        (define-variable!
          var
          (vproc env)
          env)
        'ok)))

  (define handler
    (make-handler
     'define
     eval-define
     analyze-define))

  (handler-register! handler)
  'ok)

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  (if (non-empty? (cdddr exp))
      (cadddr exp)
      '#f))

(define (install-eval-if)
  (define (eval-if exp env)
    (if (true? (my-eval (if-predicate exp) env))
        (my-eval (if-consequent exp) env)
        (my-eval (if-alternative exp) env)))
  (define (analyze-if exp)
    (let ((pproc (my-analyze (if-predicate exp)))
          (cproc (my-analyze (if-consequent exp)))
          (aproc (my-analyze (if-alternative exp))))
      (lambda (env)
        (if (true? (pproc env))
            (cproc env)
            (aproc env)))))
  (define handler
    (make-handler
     'if
     eval-if
     analyze-if))

  (handler-register! handler)
  'ok)

(define (make-begin exp-seq)
  (cons 'begin exp-seq))

(define (last-exp? seq)
  (null? (cdr seq)))

(define first-exp car)
(define rest-exps cdr)

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (install-eval-begin)
  (define begin-actions cdr)
  (define (eval-sequence exps env)
    (cond ((null? exps)
           '#f)
          ((last-exp? exps)
           (my-eval (first-exp exps) env))
          (else
           (my-eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env))))
  (define (analyze-sequence exps)
    (define (sequentially proc1 proc2)
      (lambda (env)
        (proc1 env)
        (proc2 env)))
    (define (loop first-proc rest-procs)
      (if (null? rest-procs)
          first-proc
          (loop (sequentially first-proc (car rest-procs))
                (cdr rest-procs))))
    (let ((procs (map my-analyze exps)))
      (if (null? procs)
          (const #f)
          (loop (car procs) (cdr procs)))))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (define (analyze-begin exp)
    (analyze-sequence (begin-actions exp)))

  (define handler
    (make-handler
     'begin
     eval-begin
     analyze-begin))

  (handler-register! handler)

  'ok)

(define lambda-parameters cadr)
(define lambda-body cddr)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (install-eval-lambda)
  (define (eval-lambda exp env)
    (make-procedure
     (lambda-parameters exp)
     (lambda-body exp)
     env))
  (define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
          (bproc (my-analyze (make-begin
                              (lambda-body exp)))))
      (lambda (env)
        (make-analyzed-procedure
         vars
         bproc
         env))))

  (define handler
    (make-handler
     'lambda
     eval-lambda
     analyze-lambda))

  (handler-register! handler)
  'ok)

(define cond-clauses cdr)

(define cond-predicate car)
(define cond-actions cdr)

(define (clause-arrow? clause)
  (eq? (cadr clause) '=>))

(define clause-handler caddr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              (let ((result-sym (gensym)))
                (list
                 (make-lambda
                  (list result-sym)
                  (list
                   (make-if
                    result-sym
                    (if (clause-arrow? first)
                        (list
                         (clause-handler first)
                         result-sym)
                        (sequence->exp (cond-actions first)))
                    (expand-clauses rest))))
                 (cond-predicate first)))))))
  (expand-clauses (cond-clauses exp)))

(define (install-eval-cond)
  (define (eval-cond exp env)
    (my-eval (cond->if exp) env))
  (define (analyze-cond exp)
    (my-analyze (cond->if exp)))
  (define handler
    (make-handler
     'cond
     eval-cond
     analyze-cond))

  (handler-register! handler)
  'ok)

(define (install-eval-and)
  (define (eval-and exp env)
    (define (eval-and-aux exps env)
      (cond ((null? exps) #t)
            ((null? (cdr exps))
             (my-eval (car exps) env))
            (else
              (if (true? (my-eval (car exps) env))
                (eval-and-aux (cdr exps) env)
                #f))))
    (eval-and-aux (cdr exp) env))

  (define (analyze-and exp)
    (define exps (cdr exp))
    (define analyzed-exps
      (map my-analyze exps))

    (define (analyze-aux analyzed-exps)
      (if (null? analyzed-exps)
          (const #t)
          (let ((hd (car analyzed-exps))
                (tl (cdr analyzed-exps)))
            (if (null? (cdr analyzed-exps))
                hd
                (let ((analyzed-tls (analyze-aux tl)))
                  analyzed-tls
                  (lambda (env)
                    (if (true? (hd env))
                        (analyzed-tls env)
                        #f)))))))

    (analyze-aux analyzed-exps))

  (define handler
    (make-handler
     'and
     eval-and
     analyze-and
     ))

  (handler-register! handler)
  'ok)

(define (install-eval-or)
  (define (eval-or exp env)
    (define (eval-or-aux exps env)
      (cond ((null? exps) #f)
            ((null? (cdr exps))
             (my-eval (car exps) env))
            (else
              (let ((result (my-eval (car exps) env)))
                (if (true? result)
                  result
                  (eval-or-aux (cdr exps) env))))))
    (eval-or-aux (cdr exp) env))

  (define (analyze-or exp)
    (define exps (cdr exp))
    (define analyzed-exps
      (map my-analyze exps))

    (define (analyze-aux analyzed-exps)
      (if (null? analyzed-exps)
          (const #f)
          (let ((hd (car analyzed-exps))
                (tl (cdr analyzed-exps)))
            (if (null? (cdr analyzed-exps))
                hd
                (let ((analyzed-tls (analyze-aux tl)))
                  analyzed-tls
                  (lambda (env)
                    (let ((result (hd env)))
                      (if (true? result)
                          result
                          (analyzed-tls env)))))))))
    (analyze-aux analyzed-exps))

  (define handler
    (make-handler
      'or
      eval-or
      analyze-or))

  (handler-register! handler)
  'ok)

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let->combination exp)
    (define (named-let? exp)
      (symbol? (cadr exp)))

    (define (normal-let->combination exp)
      (define let-binding-pairs (cadr exp))
      (define let-body (cddr exp))
      (define vars (map car  let-binding-pairs))
      (define exps (map cadr let-binding-pairs))

      (cons
        (make-lambda vars let-body)
        exps))

      (define (named-let->combination exp)
        (define proc-name (cadr exp))
        (define let-binding-pairs (caddr exp))
        (define let-body (cdddr exp))
        (define vars (map car  let-binding-pairs))
        (define exps (map cadr let-binding-pairs))
        (normal-let->combination
          (list 'let '()
                (cons 'define
                      (cons (cons proc-name vars)
                            let-body))
                (cons proc-name exps))))

      (if (named-let? exp)
        (named-let->combination exp)
        (normal-let->combination exp)))

(define (install-eval-let)
  (define (eval-let exp env)
    (my-eval (let->combination exp) env))
  (define (analyze-let exp)
    (my-analyze (let->combination exp)))
  (define handler
    (make-handler
     'let
     eval-let
     analyze-let))

  (handler-register! handler)
  'ok)

(define (install-eval-let*)
  (define (let*->nested-lets exp)
    (define (consume-bindings vars exps body)
      (if (null? vars)
          (make-let '() body)
          (make-let
           (list (list (car vars) (car exps)))
           (list
            (consume-bindings
             (cdr vars)
             (cdr exps)
             body)))))
    (define let*-binding-pairs (cadr exp))
    (define let*-body (cddr exp))
    (consume-bindings
     (map car  let*-binding-pairs)
     (map cadr let*-binding-pairs)
     let*-body))
  (define (eval-let* exp env)
    (my-eval (let*->nested-lets exp) env))
  (define (analyze-let* exp)
    (my-analyze (let*->nested-lets exp)))

  (define handler
    (make-handler
     'let*
     eval-let*
     analyze-let*))

  (handler-register! handler)
  'ok)

(define (install-eval-letrec)
  (define (letrec->let exp)
    (define binding-pairs
      (cadr exp))
    (define vars (map car  binding-pairs))
    (define exps (map cadr binding-pairs))
    (define body
      (cddr exp))
    (let ((bindings
           (map (lambda (var)
                  (list var ''*unassigned*))
                vars))
          (set-exprs
           (map (lambda (var exp) (list 'set! var exp))
                vars exps)))
      (append '(let)
              (list bindings)
              set-exprs
              body)))
  (define (eval-letrec exp env)
    (my-eval (letrec->let exp) env))
  (define (analyze-letrec exp)
    (my-analyze (letrec->let exp)))
  (define handler
    (make-handler
     'letrec
     eval-letrec
     analyze-letrec))

  (handler-register! handler)
  'ok)

(define (my-eval-interpret exp env)

  (define (try-simple-eval exp env)
    (cond ((self-evaluating? exp)
            (just exp))
          ((variable? exp)
            (just
              (lookup-variable-value exp env)))
          (else nothing)))

  (define (try-dispatch-eval exp env)
    (if (non-empty? exp)
      (let ((handler (my-eval-get (car exp))))
        (if handler
          (just
            (handler-eval handler exp env))
          nothing))
      nothing))

  (define (try-app-eval exp env)
    (if (application? exp)
      (just
        (my-apply
          (my-eval (operator exp) env)
          (list-of-values (operands exp) env)))
      nothing))

  ((maybe
    identity
    (lambda ()
      (error "unknown expression:" exp)))
   (or (try-simple-eval   exp env)
       (try-dispatch-eval exp env)
       (try-app-eval      exp env)
       nothing)))

(define (my-analyze exp)
  (define (try-simple-analyze exp)
    (cond ((self-evaluating? exp)
           (just (const exp)))
          ((variable? exp)
           (just
            (lambda (env)
              (lookup-variable-value exp env))))
          (else nothing)))

  (define (try-dispatch-analyze exp)
    (if (non-empty? exp)
        (let ((handler (my-eval-get (car exp))))
          (if handler
              (just
               (handler-analyze handler exp))
              nothing))
        nothing))

  (define (try-app-analyze exp)
    (if (application? exp)
        (just
         (let ((rator (my-analyze (operator exp)))
               (rands (map my-analyze (operands exp))))
           (lambda (env)
             (my-apply
              (rator env)
              (map (lambda (rand) (rand env))
                   rands)))))
        nothing))

  ((maybe
    identity
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

(define (analyze->eval analyze-xxx)
  (define (eval-xxx exp env)
    ((analyze-xxx exp) env))
  eval-xxx)

(define my-eval-analyze
  (analyze->eval my-analyze))

(define eval-approaches
  (list
   (list 'interpret
         my-eval-interpret)
   (list 'analyze
         my-eval-analyze)))

(define *my-eval-approach*
  'uninitialized)

(define my-eval
  'uninitialized)

(define (my-eval-select-approach approach)
  (display "; [my-eval]: ")
  (display "switching to approach: ")
  (display approach)
  (newline)
  (set! *my-eval-approach* approach)
  (set! my-eval
        (cadr (assoc *my-eval-approach* eval-approaches))))

(install-eval-quote)
(install-eval-define)
(install-eval-if)
(install-eval-set!)
(install-eval-begin)
(install-eval-lambda)
(install-eval-cond)
(install-eval-and)
(install-eval-or)
(install-eval-let)
(install-eval-let*)
(install-eval-letrec)

(define input-prompt "my-eval> ")
(define output-prompt "")

(define (driver-loop env)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop env))

(define prompt-for-input display)

(define announce-output display)

(define (user-print object)
  (if (proc-compound? object)
    (out (list 'proc-compound
               (proc-vars object)
               (proc-body object)
               '<proc-env>))
    (out object)))

(define (my-eval-start-using-approach approach)
  (my-eval-select-approach approach)
  (driver-loop (init-env)))

(define (my-eval-start)
  (my-eval-start-using-approach 'analyze))
