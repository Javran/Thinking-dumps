; require: make-lambda

(define (install-eval-define)

  ; form #1: (define <var> <val>)
  ; form #2: (define (proc-name . args) <body>)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
      (cadr exp)
      (caddr exp)))

  (define (definition-value exp)
    (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

  (define (eval-define exp env)
    (define-variable!
      (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  (define (test)
    'todo)

  (define handler
    (make-handler
      'define
      eval-define
      test))

  (handler-register! handler)
  'done)
