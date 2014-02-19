; handle lambda expressions

; (lambda <vars> <body>)
; vars: a list of variables
; body: a sequenence of expressions,
;         represented as a list

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
      (make-procedure
       vars
       ; TODO: analyze exp
       (lambda-body exp)
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

    ;; we might need to bring back function application
    ;; before we can run tests here.
    'analyze-app)

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
;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:
