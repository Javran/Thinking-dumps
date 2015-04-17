;; a standalone version of metacircular evaluator
;; TODO: not standalone
;; TODO: we should better have a list of assumed
;; operations somewhere
;; TODO: too many testcases, should be simplified

;; TODO: eliminate all use of "format"
;; to make this portable

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

(load "../common/test-utils.scm")
;; TODO: remove tests - we can test it against the evaluator
;; in our implementing language.
(define *my-eval-do-test* #t)

(define (make-handler
         slot
         proc-eval
         proc-analyze
         test-proc)
  (list 'handler
        slot
        proc-eval
        proc-analyze
        test-proc))

(define (handler? h)
  (and (list? h)
       (not (null? h))
       (eq? (car h) 'handler)))

(define handler-slot cadr)
(define handler-proc-eval caddr)
(define handler-proc-analyze cadddr)
(define handler-test (compose car cddddr))

;; TODO: remove
(define (handler-run-test h)
  (if (handler-test h)
    ((handler-test h))
    'no-test-available))

(define (handler-eval handler exp env)
  ((handler-proc-eval handler) exp env))

(define (handler-analyze handler exp)
  ((handler-proc-analyze handler) exp))

(define (handler-register! h)
  (my-eval-put! (handler-slot h) h))

(define my-eval-get #f)
(define my-eval-put! #f)
(define my-eval-test-installed-handlers #f)
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

  (define (test-slot slot alist)
    (for-each
     display
     '("Testing " slot " "))
    (define handler
      (get-handler slot alist))
    (assert handler "handler not found")
    (define result
      (handler-run-test handler))
    (newline)
    (display "  Result: ")
    (display result)
    (newline)
    result)

  (define (test-all-slots)
    (define slots
      (map car eval-handler-alist))
    (newline)
    (define results
      (map (lambda (slot)
             (test-slot slot eval-handler-alist))
           slots))
    (define not-ok
      (map
       car
       (filter
        (lambda (pair)
          (not (eq? (cdr pair) 'ok)))
        (map cons slots results))))
    (out "Summary: slots that did not return with 'ok: ")
    (display "  ") (display not-ok) (newline)
    (out "Test done.")
    'ok)

  (set! my-eval-get get)
  (set! my-eval-put! put!)
  (set! my-eval-test-installed-handlers test-all-slots)
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

(define (test-both test-eval-xxx
                   eval-xxx
                   analyze-xxx)
  (lambda ()
    (let ((result
           (list
            (test-eval-xxx eval-xxx)
            (test-eval-xxx (analyze->eval analyze-xxx)))))
      (if (equal? result '(ok ok))
          'ok
          result))))

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

(define (test-eval-simple)
  (let ((testcases
          (list
            (mat 'abc #f)
            (mat 1    #t)
            (mat 1.5  #t)
            (mat '(1 2 3) #f)
            (mat "a"  #t)
            (mat #\a  #t))))
    (do-test self-evaluating? testcases))
  (let ((testcases
          (list
            (mat 'a   #t)
            (mat '(a) #f)
            (mat 10   #f))))
    (do-test variable? testcases))
  'ok)

(if *my-eval-do-test*
  (test-eval-simple))

(define quoted?
  (list-tagged-with 'quote))

(define text-of-quotation cadr)

(define (install-eval-quote)

  (define (eval-quote exp env)
    (text-of-quotation exp))

  (define (analyze-quote exp)
    (let ((result (text-of-quotation exp)))
      (const result)))

  (define (test-eval eval-quote)
    (let ((testcases
           (list
            (mat '(quote a) #f 'a)
            (mat '(quote "a") #f "a")
            (mat '(quote 1) #f 1))))
      (do-test eval-quote testcases))
    'ok)

  (define handler
    (make-handler
      'quote
      eval-quote
      analyze-quote
      (test-both
       test-eval
       eval-quote
       analyze-quote)))

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

  (define (test-eval eval-set!)
    (define env
      (extend-environment
        (list 'a 'b 'c)
        (list 1 2 3)
        the-empty-environment))
    (define env1
      (extend-environment
        (list 'c 'd 'e)
        (list #\c #\d #\e)
        env))
    (define env2
      (extend-environment
        (list 'd 'e 'f)
        (list "d" "e" "f")
        env1))
    (define env3
      (extend-environment
        (list 'a 'b 'c)
        (list "a3" "b3" "c3")
        env1))

    (eval-set! '(set! a "ax") env3)

    (do-test
      lookup-variable-value
      (list
        (mat 'a env  1)
        (mat 'a env1 1)
        (mat 'a env2 1)
        (mat 'a env3 "ax"))
      equal?)

    (eval-set! '(set! a "ay") env1)

    (do-test
      lookup-variable-value
      (list
        (mat 'a env  "ay")
        (mat 'a env1 "ay")
        (mat 'a env2 "ay")
        (mat 'a env3 "ax"))
      equal?)
    'ok)

  (define handler
    (make-handler
      'set!
      eval-set!
      analyze-set!
      (test-both
       test-eval
       eval-set!
       analyze-set!)))

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

  (define (test-eval eval-define)
    (define env
      (extend-environment
        (list 'a) (list 10) the-empty-environment))

    (define env1
      (extend-environment
        '() '() env))

    (eval-define '(define a 1) env)
    (eval-define '(define b 2) env)

    (do-test
      lookup-variable-value
      (list
        (mat 'a env 1)
        (mat 'b env 2)
        (mat 'a env1 1)
        (mat 'b env1 2)))

    (eval-define '(define a "aaa") env1)
    (eval-define '(define b "bbb") env1)
    (eval-define '(define c "ccc") env1)

    (do-test
      lookup-variable-value
      (list
        (mat 'a env 1)
        (mat 'b env 2)
        (mat 'a env1 "aaa")
        (mat 'b env1 "bbb")
        (mat 'c env1 "ccc")))

    (eval-define
      '(define (proc-branch a b c)
         (if a b c))
      env)

    (eval-define
      '(define (proc-const a)
         12345)
      env)

    (do-test
      my-eval
      (list
        (mat '(proc-branch #t 1 2) env 1)
        (mat '(proc-branch #f 1 2) env 2)
        (mat '(proc-const 0) env 12345)
        (mat '(proc-const #t) env 12345)
        ))

    'ok)

  (define handler
    (make-handler
      'define
      eval-define
      analyze-define
      (test-both
       test-eval
       eval-define
       analyze-define)))

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

  (define (test-eval eval-if)
    (define env
      (init-env))

    (define testcases
      (list
        (mat '(if 1 10) env 10)
        (mat '(if 1 10 20) env 10)
        (mat '(if #t 10 20) env 10)
        (mat '(if 'a 10 20) env 10)
        (mat '(if '#f 10 20) env 20)
        (mat '(if #f 10 20) env 20)
        (mat '(if true 10 20) env 10)
        (mat '(if 'false 10 20) env 10)
        (mat '(if false 10 20) env 20)
        (mat '(if (= 1 1) (+ 10 20) (* 10 20)) env 30)
        (mat '(if (= 0 1) (+ 10 20) (* 10 20)) env 200)
        ))
    (do-test eval-if testcases)
    'ok)

  (define handler
    (make-handler
      'if
      eval-if
      analyze-if
      (test-both
       test-eval
       eval-if
       analyze-if)))

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

  (define (test-eval eval-begin)
    (define env
      (extend-environment
        (list 'a 'b 'c)
        (list 1 2 3)
        the-empty-environment))

    (define testcases
      (list
        (mat '(begin) env #f)
        (mat '(begin a) env 1)
        (mat '(begin 1 2 3) env 3)
        (mat '(begin a b b b c c c) env 3)
        (mat '(begin 30 (if #t 10 20)) env 10)
        (mat '(begin 30 (if #f 10 20)) env 20)
        (mat '(begin (if #t 10 20) 30) env 30)
        (mat '(begin (if #f 10 20) 30) env 30)
        ))
    (do-test eval-begin testcases)
    'ok)

  (define handler
    (make-handler
      'begin
      eval-begin
      analyze-begin
      (test-both
       test-eval
       eval-begin
       analyze-begin)))

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

  (define (test-eval eval-lambda)
    (define env
      the-empty-environment)

    (define proc-branch-exp
      '(lambda (a b c)
         (if a b c)))
    (define proc-const-fun-exp
      '(lambda (a)
         (lambda (b)
           a)))

    (define testcases
      (list
        (mat (make-application
               proc-branch-exp
               (list #t 10 20)) env 10)
        (mat (make-application
               proc-branch-exp
               (list #f 10 20)) env 20)
        (mat (make-application
               (make-application
                 proc-const-fun-exp
                 (list 10))
               (list 20)) env 10)
        (mat (make-application
               (make-application
                 proc-const-fun-exp
                 (list 10))
               (list "aaaa")) env 10)
        ))

    (do-test my-eval testcases)
    'ok)

  (define handler
    (make-handler
      'lambda
      eval-lambda
      analyze-lambda
      (test-both
       test-eval
       eval-lambda
       analyze-lambda
       )))

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

  (define (test-eval eval-cond)
    (define env
      (init-env))

    (define cond-test-exp-1
      `(cond ((= a 0) 2)
             ((= a 1) 1)
             ((= a 2) 0)))

    (define cond-test-exp-2
      `(cond ((= a 0) 10)
             (else 10 15 20)))

    (define cond-test-exp-3
      `(cond ((= a 0) => (lambda (x) (if x 10 20)))
             ((= a 1) => (lambda (x) (if x 30 40)))
             ((= a 2) 50)
             (else 60)))

    (do-test
      eval-cond
      (list
        (mat cond-test-exp-1
             (extend-environment
               '(a) '(0) env) 2)
        (mat cond-test-exp-1
             (extend-environment
               '(a) '(1) env) 1)
        (mat cond-test-exp-1
             (extend-environment
               '(a) '(2) env) 0)
        (mat cond-test-exp-2
             (extend-environment
               '(a) '(0) env) 10)
        (mat cond-test-exp-2
             (extend-environment
               '(a) '(1) env) 20)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(0) env) 10)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(1) env) 30)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(2) env) 50)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(3) env) 60)
        ))
    'ok)

  (define handler
    (make-handler
      'cond
      eval-cond
      analyze-cond
      (test-both
       test-eval
       eval-cond
       analyze-cond)))

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

  (define (test-eval eval-and)
    (let ((env (init-env)))
      (do-test
        eval-and
        (list
          (mat '(and) env #t)
          (mat '(and (= 1 1) (= 2 2)) env #t)
          (mat '(and (= 1 1) #f (error 'wont-reach)) env #f)
          (mat '(and 1 2 3 4) env 4)
          (mat '(and 1) env 1)
          ))
      'ok))

  (define handler
    (make-handler
      'and
      eval-and
      analyze-and
      (test-both
       test-eval
       eval-and
       analyze-and)))

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

  (define (test-eval eval-or)
    (let ((env (init-env)))
      (do-test
        eval-or
        (list
          (mat '(or) env #f)
          (mat '(or #f) env #f)
          (mat '(or #t (error 'wont-reach)) env #t)
          (mat '(or (< 1 1) (> 2 2)) env #f)
          (mat '(or (quote symbol)) env 'symbol)
          (mat '(or #f #f 1) env 1)
          ))
      'ok))

  (define handler
    (make-handler
      'or
      eval-or
      analyze-or
      (test-both
       test-eval
       eval-or
       analyze-or)))

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

  (define (test-eval eval-let)
    (let ((env (init-env)))
      (do-test
        eval-let
        (list
          (mat `(let ((x 1)
                      (y 2)
                      (z 3))
                  (+ x y z)) env 6)
          (mat `(let ((a 10)
                      (b 20))
                  (+ a a)
                  (* b a)) env 200)
          (mat `(let ()
                  10) env 10)
          (mat `(let ((a 10))
                  (let ((a (+ a 20)))
                    (let ((a (* a 30)))
                      (+ a a)))) env 1800)
          (mat `(let fib-iter ((a 1) (b 0) (count 10))
                  (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))) env 55)
          (mat `(let fib-iter ((a 1) (b 0) (count 14))
                  (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))) env 377)
          (mat `(let proc ((i 1) (acc 1))
                  (if (<= i 10) (proc (+ i 1) (* i acc)) acc)) env 3628800)
          (mat `(let proc ((i 1) (acc 0))
                  (if (<= i 100) (proc (+ i 1) (+ i acc)) acc)) env 5050)
          ))
      'ok))

  (define handler
    (make-handler
      'let
      eval-let
      analyze-let
      (test-both
       test-eval
       eval-let
       analyze-let)))

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

  (define (test-eval eval-let*)
    (let ((env (init-env)))
      (do-test
        eval-let*
        (list
          (mat `(let* ((x 1)
                       (y (+ 1 x))
                       (z (* 2 y)))
                  (+ x y z)) env 7)
          (mat `(let* ((x 3)
                       (y (+ x 2))
                       (z (+ x y 5)))
                  (* x z)) env 39)))
      'ok))

  (define handler
    (make-handler
      'let*
      eval-let*
      analyze-let*
      (test-both
       test-eval
       eval-let*
       analyze-let*)))

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
    `(let ,(map (lambda (var)
                  `(,var '*unassigned*))
                vars)
       ,@(map (lambda (var exp) `(set! ,var ,exp))
              vars exps)
       ,@body))

  (define (eval-letrec exp env)
    (my-eval (letrec->let exp) env))

  (define (analyze-letrec exp)
    (my-analyze (letrec->let exp)))

  (define (test-eval eval-letrec)
    (let ((env (init-env)))
      (do-test
        eval-letrec
        (list
          (mat `(letrec ((fact (lambda (n)
                                 (if (= n 0)
                                   1
                                   (* n (fact (- n 1)))))))
                  (fact 4)) env
               24)
          (mat `(letrec ((f1 (lambda (n)
                               (if (= n 0)
                                 1
                                 (* n (f2 (- n 1))))))
                         (f2 (lambda (n)
                               (if (= n 0)
                                 1
                                 (* n (f3 (- n 1))))))
                         (f3 (lambda (n)
                               (if (= n 0)
                                 1
                                 (* n (f1 (- n 1)))))))
                  (f3 10)) env
               3628800)
          ))
      'ok))

  (define handler
    (make-handler
      'letrec
      eval-letrec
      analyze-letrec
      (test-both
       test-eval
       eval-letrec
       analyze-letrec)))

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

(newline)
(define (my-eval-test-all)
  (for-each
   (lambda (approach)
     (begin
       (my-eval-select-approach approach)
       (my-eval-test-installed-handlers)
       ))
   (map car eval-approaches)))

(if *my-eval-do-test*
    (my-eval-test-all)
    'skipped)

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
