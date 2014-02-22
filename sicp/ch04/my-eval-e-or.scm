; handle or-exp
; from ./exercise 4_4_a.scm

(define (install-eval-or)
  (define (eval-or exp env)
    ; evaluate expressions
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

  ;; should be similiar to the implementation of `eval-and`.
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
                  ;; force it, just making sure.
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
;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:
