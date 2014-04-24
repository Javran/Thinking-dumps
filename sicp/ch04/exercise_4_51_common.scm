(define (install-amb-permanent-set!)

  (define (analyze-permanent-set! exp)
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
                            fail2)))
               fail))))

  (define (test)
    (let ((env (init-env)))
    (do-test
     amb-eval-all
     (list
      ;; we only need to verify that
      ;; the side effect is permanently set.
      ;; which cannot be undone by calling the "fail" program
      (mat `(begin
              (define count 0)
              (amb 'a 'b 'c 'd)
              (permanent-set! count (+ count 1))
              count)
           env
           `(1 2 3 4))
           )))
    'ok)

  (define handler
    (make-amb-handler
     'permanent-set!
     analyze-permanent-set!
     test))

  (ahandler-register! handler)
  'ok)
