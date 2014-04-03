(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
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
