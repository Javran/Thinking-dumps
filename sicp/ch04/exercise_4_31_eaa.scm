;; eea: extended argument annotation

;; (define-eea <proc-arg-list> <body>)
(define define-eea-proc-arg-list cadr)

(define define-eea-proc-name
  (compose
   car
   define-eea-proc-arg-list))

(define define-eea-args
  (compose
   cdr
   define-eea-proc-arg-list))

(define define-eea-body cddr)

(define (install-eval-define-eea)

  ;; structure:
  ;; (list 'proc-eea <annotations> <proc>)
  ;; annotations: (list (list <var-name> <call-strategy>))
  ;; var-name: symbol
  ;; call-strategy: 'call-by-value 'call-by-name 'call-by-need
  ;; proc: a lambda
  (define (define-eea->define exp)

    'todo)


  (define (eval-define-eea exp env)
    (my-eval (define-eea->define exp) env))

  (define (analyze-define-eea exp)
    (my-analyze (define-eea->define exp)))

  (define (test-eval eval-define-eea)

    (define sample
      `(define-eea (f a (b lazy) c (d lazy-memo))
         (foo bar bar c)
         (nested foo
                 (nested a b c)
                 d)))
    (out "sample test...")

    (out
     (define-eea-proc-arg-list
       sample)
     (define-eea-proc-name
       sample)
     (define-eea-args
       sample)
     (define-eea-body
       sample))

    'todo)

  (define handler
    (make-handler
     'define-eea
     eval-define-eea
     analyze-define-eea
     (test-both
      test-eval
      eval-define-eea
      analyze-define-eea)))

  (handler-register! handler)
  'ok)
;; Local variables:
;; proc-entry: "./exercise_4_31.scm"
;; End:
