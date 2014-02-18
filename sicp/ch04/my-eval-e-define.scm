; Local variables:
; proc-entry: "./my-eval.scm"
; End:

; form #1: (define <var> <val>)
; form #2: (define (proc-name . args) <body>)
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

; require: quote
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
    ; 2-layer
    (define env
      (extend-environment
        (list 'a) (list 10) the-empty-environment))

    (define env1
      (extend-environment
        '() '() env))

    ; define + redefine on `env`
    (eval-define '(define a 1) env)
    (eval-define '(define b 2) env)

    (do-test
      lookup-variable-value
      (list
        (mat 'a env 1)
        (mat 'b env 2)
        (mat 'a env1 1)
        (mat 'b env1 2)))

    ; define / shadowing on `env1`
    (eval-define '(define a "aaa") env1)
    (eval-define '(define b "bbb") env1)
    (eval-define '(define c "ccc") env1)

    ; changes on `env1` won't effect
    ;   anything on `env`
    (do-test
      lookup-variable-value
      (list
        (mat 'a env 1)
        (mat 'b env 2)
        (mat 'a env1 "aaa")
        (mat 'b env1 "bbb")
        (mat 'c env1 "ccc")))

    ;; (eval-define
    ;;   '(define (proc-branch a b c)
    ;;      (if a b c))
    ;;   env)

    ;; (eval-define
    ;;   '(define (proc-const a)
    ;;      12345)
    ;;   env)

    ;; (do-test
    ;;   my-eval
    ;;   (list
    ;;     (mat '(proc-branch #t 1 2) env 1)
    ;;     (mat '(proc-branch #f 1 2) env 2)
    ;;     (mat '(proc-const 0) env 12345)
    ;;     (mat '(proc-const #t) env 12345)
    ;;     ))

    'analyze-need-lambda)

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
