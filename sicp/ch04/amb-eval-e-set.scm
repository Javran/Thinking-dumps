(load "./my-eval-e-set.scm")
(load "./amb-eval-test.scm")

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (amb-analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          ;; when the attempt is failed
                          ;; `set!` now restore the old value
                          (lambda ()
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

(define (install-amb-set!)

  (define analyze-set!
    analyze-assignment)

  (define (test)
    (let ((env (init-env)))
      (test-eval `(define x #f) env)
      (test-eval `(define y #f) env)
      (test-eval `(define f #f) env)

      (test-eval `(set! x 20) env)
      (test-eval `(set! f (lambda (f) (* f f))) env)
      (test-eval `(set! y (f x)) env)

      (do-test
       test-eval
       (list
        (mat `x env 20)
        (mat `y env 400))
       (test-compare equal?))

      (do-test
       amb-eval-all
       (list
        ;; set! cannot be permanent
        (mat `(begin
                (define count 0)
                (set! count (+ count 1))
                (amb 'a 'b 'c 'd)
                count)
             env
             '(1 1 1 1))))
      'ok))

  (define handler
    (make-amb-handler
     'set!
     analyze-set!
     test))

  (ahandler-register! handler)
  'ok)
